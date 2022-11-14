--1 Extensions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

--This is to work not only with Strings
{-# LANGUAGE OverloadedStrings   #-}

-- required to use custom data types
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE MultiParamTypeClasses      #-} 

module GigsplitterOnChain where

-- Haskell Imports
import qualified Prelude                                         as P

-- Plutus Imports
import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
-- import qualified Prelude                                         as P
import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash)
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada

-- In case the show is canceled, money goes back to venue
-- Manager will get 20% of amountDeposited
-- List of all band members, who will get the remaining ADA after Manager is paid
data EscrowDetails = EscrowDetails -- This specifies who we payout to and how much ADA is stored in escrow
    {
        venue             :: Ledger.PaymentPubKeyHash, 
        manager           :: Ledger.PaymentPubKeyHash,
        bassPlayer        :: Ledger.PaymentPubKeyHash,
        drummer           :: Ledger.PaymentPubKeyHash,
        guitarPlayer      :: Ledger.PaymentPubKeyHash,
        singer            :: Ledger.PaymentPubKeyHash,
        showTime          :: V2LedgerApi.POSIXTime,
        amountDeposited   :: P.Integer
    } deriving P.Show

-- data BenParam = BenParam
--     {
--     creator :: Ledger.PaymentPubKeyHash,
--     beneficiary :: Ledger.PaymentPubKeyHash,
--     deadline :: V2LedgerApi.POSIXTime
--     } 

PlutusTx.unstableMakeIsData ''EscrowDetails
PlutusTx.makeLift ''EscrowDetails

newtype Redeem = Redeem
    {
        redeem :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem -- This is to instantiate the IsData class
PlutusTx.makeLift ''Redeem

data Dat = Dat 
    {
        showId :: Integer -- showId
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat
PlutusTx.makeLift ''Dat

-- data PayoutDetails = PayoutDetails -- This specifies who we payout to and how much ADA is stored in escrow
--     {
--         payee             :: Ledger.PaymentPubKeyHash,
--         paymentDate       :: V2LedgerApi.POSIXTime,
--         amountPaid        :: P.Integer,
--         showId            :: P.Integer
--     } deriving P.Show

-- PlutusTx.unstableMakeIsData ''PayoutDetails
-- PlutusTx.makeLift ''PayoutDetails

-- Just a Test Validator that always succeeds to test my OffChain code
-- {-# INLINEABLE depositV #-}
-- depositV :: EscrowDetails -> Dat -> Redeem -> Contexts.ScriptContext -> Bool
-- depositV depositp d r context = 
--     traceIfFalse "This should not actually get printed" True -- 
--     where
--         txinfo :: Contexts.TxInfo
--         txinfo = Contexts.scriptContextTxInfo context

-- Deposit Validator
{-# INLINEABLE depositV #-}
depositV :: EscrowDetails -> Dat -> Redeem -> Contexts.ScriptContext -> Bool
depositV depositp d r context = 
    -- traceIfFalse "This should not actually get printed" True -- Always pass during dev
    traceIfFalse "Sorry the show ID does not match" (showId d == redeem r)
    -- traceIfFalse "Wrong pubkeyhash" signedByBeneficiary
    -- traceIfFalse "Deadline not yet reached" deadlinepassed 
    && traceIfFalse "Not paid royalties" calculateRoyalties
    where
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context

        -- signedByBeneficiary :: Bool
        -- signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (recipientManager depositp)

        -- deadlinepassed :: Bool
        -- deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (contractTimestamp depositp)) (Contexts.txInfoValidRange txinfo)
        
        adaroyalties :: Maybe Ada.Ada
        adaroyalties = do
            validatedValue <- Contexts.txOutValue . Contexts.txInInfoResolved <$> Contexts.findOwnInput context
            Just $ Ada.fromValue validatedValue `Ada.divide` 20

        getValuePaidToManager :: Ada.Ada
        getValuePaidToManager = Ada.fromValue $ Contexts.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (manager depositp))

        compareValues :: Ada.Ada -> Maybe Ada.Ada -> Bool
        -- compareValues Nothing _ = False
        compareValues vToCreator adaTx = Just (vToCreator) >= adaTx

        calculateRoyalties :: Bool
        calculateRoyalties = compareValues (getValuePaidToManager) (adaroyalties)

data DepositType
instance V2UtilsTypeScripts.ValidatorTypes DepositType where
    type instance RedeemerType DepositType = Redeem
    type instance DatumType DepositType = Dat

-- depositTypeV :: EscrowDetails -> V2UtilsTypeScripts.TypedValidator DepositType
-- depositTypeV depositp = V2UtilsTypeScripts.mkTypedValidator @DepositType 
--     ($$(compile [|| depositV ||]) `PlutusTx.applyCode` PlutusTx.liftCode depositp)
--     $$(compile [|| wrap ||]) where
--         wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

depositTypeV :: EscrowDetails -> V2UtilsTypeScripts.TypedValidator DepositType
depositTypeV depositp = V2UtilsTypeScripts.mkTypedValidator @DepositType 
    ($$(compile [|| depositV ||]) `PlutusTx.applyCode` PlutusTx.liftCode depositp)
    $$(compile [|| wrap ||]) where
        wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: EscrowDetails -> V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript . depositTypeV

validatorHash :: EscrowDetails -> V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash . depositTypeV

address :: EscrowDetails -> V1LAddress.Address
address = V1LAddress.scriptHashAddress . validatorHash