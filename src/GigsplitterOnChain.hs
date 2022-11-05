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
-- import qualified Ledger.Ada                                      as Ada


data EscrowDetails = EscrowDetails -- This specifies who we payout to and how much ADA is stored in escrow
    {
        recipientVenue    :: Ledger.PaymentPubKeyHash, -- In case the show is canceled, money goes back to venue
        recipientManager  :: Ledger.PaymentPubKeyHash, -- Manager will get 20% of amountDeposited
        bandMembers       :: [Ledger.PaymentPubKeyHash], -- List of all band members, who will get the remaining ADA after Manager is paid
        paymentDeadline   :: V2LedgerApi.POSIXTime,
        amountDeposited   :: P.Integer,
        showId            :: P.Integer
    } deriving P.Show

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
        ddata :: Integer -- showId
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat
PlutusTx.makeLift ''Dat

{-# INLINEABLE depositV #-}
depositV :: EscrowDetails -> Dat -> Redeem -> Contexts.ScriptContext -> Bool
depositV depositp d r context = 
    traceIfFalse "Sorry the guess is not correct" (ddata d == redeem r) &&
    traceIfFalse "Wrong pubkeyhash" signedByBeneficiary &&
    traceIfFalse "Deadline not yet reached"  deadlinepassed 
    -- && traceIfFalse "Not paid royalties"  calculateRoyalties
    where
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context

        signedByBeneficiary :: Bool
        signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (recipientManager depositp)

        deadlinepassed :: Bool
        deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (paymentDeadline depositp)) (Contexts.txInfoValidRange txinfo)
        
        -- adaroyalties :: Maybe Ada.Ada
        -- adaroyalties = do
        --     validatedValue <- Contexts.txOutValue . Contexts.txInInfoResolved <$> Contexts.findOwnInput context
        --     Just $ Ada.fromValue validatedValue `Ada.divide` 10

        -- getValuePaidToCreator :: Ada.Ada
        -- getValuePaidToCreator = Ada.fromValue $ Contexts.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (creator depositp))

        -- compareValues :: Ada.Ada -> Maybe Ada.Ada -> Bool
        -- -- compareValues Nothing _ = False
        -- compareValues vToCreator adaTx = Just (vToCreator) >= adaTx

        -- calculateRoyalties :: Bool
        -- calculateRoyalties = compareValues (getValuePaidToCreator) (adaroyalties)

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