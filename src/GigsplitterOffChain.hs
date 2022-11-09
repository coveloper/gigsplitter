{-# LANGUAGE DataKinds #-} --Makes the kind system extensible
{-# LANGUAGE TemplateHaskell #-} --allows you to do type-safe compile-time meta-programming
{-# LANGUAGE NoImplicitPrelude   #-} --PlutusTx prelude has priority over Haskell prelude
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-} --required to use custo datatypes
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards #-} -- To allow notation like Getparams {..}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module GigsplitterOffChain where

-- Haskell Imports
import qualified Control.Monad            as Monad (void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Map                 as Map

-- Plutus Imports
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.Contract          as PlutusContract
import qualified Ledger.Ada               as Ada
import qualified Ledger.Tx                as LedgerTx
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Ledger                                          (PaymentPubKeyHash, Value)
import qualified Text.Printf              as TextPrintf (printf)
import qualified Ledger.Constraints       as Constraints
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger
import qualified Data.Void                as Void (Void)
import qualified Data.Text as T
import Plutus.Contract (Contract, Endpoint, Promise, endpoint, logInfo, selectList, submitTxConstraints,
                        submitTxConstraintsSpending, type (.\/), utxosAt)


import qualified GigsplitterOnChain as OnChain

data DepositParams =
    DepositParams
        { venuePerson       :: Ledger.PaymentPubKeyHash,
          managerPerson     :: Ledger.PaymentPubKeyHash, 
          singerPerson      :: Ledger.PaymentPubKeyHash,
          bassPerson        :: Ledger.PaymentPubKeyHash, 
          drumsPerson       :: Ledger.PaymentPubKeyHash,
          guitarPerson      :: Ledger.PaymentPubKeyHash,
          paymentDeadline   :: LedgerApiV2.POSIXTime,
          amountDeposited   :: P.Integer,
          showId            :: P.Integer
        }
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)
PlutusTx.unstableMakeIsData ''DepositParams
-- PlutusTx.makeLift ''DepositParams

-- data PayoutParams = PayoutParams 
--     {
--         ppShowId              :: Integer
--     } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON)
data PayoutParams =
    PayoutParams
        { ppVenuePerson       :: Ledger.PaymentPubKeyHash,
          ppManagerPerson     :: Ledger.PaymentPubKeyHash, 
          ppSingerPerson      :: Ledger.PaymentPubKeyHash,
          ppBassPerson        :: Ledger.PaymentPubKeyHash, 
          ppDrumsPerson       :: Ledger.PaymentPubKeyHash,
          ppGuitarPerson      :: Ledger.PaymentPubKeyHash,
          ppPaymentDeadline   :: LedgerApiV2.POSIXTime,
          ppAmountDeposited   :: P.Integer,
          ppShowId            :: P.Integer
        }
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

PlutusTx.unstableMakeIsData ''PayoutParams

type GigSchema =
    PlutusContract.Endpoint "Deposit" DepositParams
    PlutusContract..\/ PlutusContract.Endpoint "Payout" PayoutParams

-- deposit
deposit :: PlutusContract.AsContractError e => DepositParams -> PlutusContract.Contract w s e ()
deposit dp = do
    let p = OnChain.EscrowDetails -- Populate OnChain.EscrowDetails with values from DepositParams
            {
                OnChain.recipientVenue    = venuePerson dp,
                OnChain.recipientManager  = managerPerson dp, 
                OnChain.bandMembers       = [singerPerson dp,
                                             bassPerson dp,
                                             drumsPerson dp,
                                             guitarPerson dp],
                OnChain.paymentDeadline   = paymentDeadline dp,
                OnChain.amountDeposited   = amountDeposited dp
            }
        d = OnChain.Dat { OnChain.showId = showId dp}
        v = Ada.lovelaceValueOf P.$ amountDeposited dp
        txConstraints = Constraints.mustPayToOtherScript (OnChain.validatorHash p) (LedgerApiV2.Datum P.$ PlutusTx.toBuiltinData d) v
        lookups = Constraints.plutusV2OtherScript P.$ OnChain.validator p
        scriptAddress = OnChain.address p
        scriptHash = OnChain.validatorHash p


-- the final goal is to build and submit the transaction
    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.DepositType lookups txConstraints
    Monad.void P.$ PlutusContract.awaitTxConfirmed P.$ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Start Endpoint - Submited - Datum: %s - Value: %s ---------------------------" (P.show d) (P.show v)
    PlutusContract.logInfo @P.String P.$ TextPrintf.printf "--------------------------- Script with Address: %s and Hash: %s ---------------------------" (P.show scriptAddress) (P.show scriptHash)

-- Payout
payout :: forall w s. PayoutParams -> PlutusContract.Contract w s T.Text ()
payout PayoutParams{..} = do
    requestor <- PlutusContract.ownFirstPaymentPubKeyHash
    now <- PlutusContract.currentTime
    -- if now < ppPaymentDeadline
    if now > ppPaymentDeadline -- make sure we are calling payout AFTER the deadline (show date)
        then PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline not yet reached - Deadline: %s - Now: %s" (P.show ppPaymentDeadline) (P.show now)
        else do
            let param = OnChain.EscrowDetails {
                        OnChain.recipientVenue    = ppVenuePerson,
                        OnChain.recipientManager  = ppManagerPerson, 
                        OnChain.bandMembers       = [ppSingerPerson,
                                                    ppBassPerson,
                                                    ppDrumsPerson,
                                                    ppGuitarPerson],
                        OnChain.paymentDeadline   = ppPaymentDeadline,
                        OnChain.amountDeposited   = ppAmountDeposited
                }
                r = OnChain.Redeem { OnChain.redeem = ppShowId }
        --utxos <- PlutusContract.utxosAt $ OnChain.address param
        -- utxos <- findUtxoInValidator param ppShowId
        -- if Map.null utxos
        -- then PlutusContract.logInfo @P.String $ "No utxos found"
        -- else PlutusContract.logInfo @P.String $ "utxos Available"
            maybeutxo <- findUtxoInValidator param ppShowId --finds the utxos associated to the beneficiary that have valid deadline and guess number
            case maybeutxo of
                Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Wrong ShowId %s or not deadline reached %s or wrong beneficiary" (P.show r) (P.show $ now)
                Just (oref, o) -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
                    let lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                                  Constraints.plutusV2OtherScript (OnChain.validator param)
                        tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                            Constraints.mustValidateIn (LedgerApiV2.from now) P.<>
                            Constraints.mustPayToPubKey ppManagerPerson (getTotalValuePay o) -- We need to pay everone
                    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.DepositType lookups tx
                    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
                    PlutusContract.logInfo @P.String $ "Payout complete"

getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.Dat
getDatum (_, o) = do
    let 
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum o

    LedgerApiV2.Datum e <- snd datHashOrDatum
    
    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.Dat) of    
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Integer -> Bool
checkUTXO (oref,o) n = do
    case getDatum (oref,o) of
        Nothing -> False
        Just OnChain.Dat{..}
            | showId == n -> True
            | otherwise  -> False

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Integer -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUTXO [] _ = Nothing
findUTXO [(oref,o)] n = do
    if checkUTXO (oref, o) n then 
        return (oref, o)
    else 
        Nothing
findUTXO ((oref,o):xs) n
    | checkUTXO (oref ,o)  n = return (oref, o)
    | otherwise = findUTXO xs n

findUtxoInValidator :: OnChain.EscrowDetails -> Integer -> PlutusContract.Contract w s T.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidator gparam n = do
    utxos <- PlutusContract.utxosAt $ OnChain.address gparam
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs n
    return out

getTotalValuePay :: LedgerTx.ChainIndexTxOut -> Ledger.Value
getTotalValuePay o = do
    Ada.toValue $ (Ada.fromValue $ LedgerTx._ciTxOutValue o) `Ada.divide` 10
    -- return tValue


-- This puts all together. The select means to offer selection to the user. 
endpoints :: PlutusContract.Contract () GigSchema T.Text ()
endpoints = PlutusContract.awaitPromise (deposit' `PlutusContract.select` payout') >> endpoints
    where 
        deposit' = PlutusContract.endpoint @"Deposit" deposit
        payout' = PlutusContract.endpoint @"Payout" payout

-- endpoints :: PlutusContract.Contract () GigSchema T.Text ()
-- -- BLOCK10

-- endpoints = PlutusContract.selectList [deposit, unlock]