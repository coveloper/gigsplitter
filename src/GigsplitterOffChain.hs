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
        { venuePkh          :: !Ledger.PaymentPubKeyHash,
          managerPkh        :: !Ledger.PaymentPubKeyHash,
          bassPlayerPkh     :: !Ledger.PaymentPubKeyHash,
          drummerPkh        :: !Ledger.PaymentPubKeyHash,
          guitarPlayerPkh   :: !Ledger.PaymentPubKeyHash,
          singerPkh         :: !Ledger.PaymentPubKeyHash,
          showTime          :: !LedgerApiV2.POSIXTime,
          amountDeposited   :: !P.Integer,
          showId            :: !P.Integer
        }
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)
PlutusTx.unstableMakeIsData ''DepositParams

data PayoutParams =
    PayoutParams
        { ppVenuePkh            :: !Ledger.PaymentPubKeyHash,
          ppBassPlayerPkh       :: !Ledger.PaymentPubKeyHash,
          ppDrummerPkh          :: !Ledger.PaymentPubKeyHash,
          ppGuitarPlayerPkh     :: !Ledger.PaymentPubKeyHash,
          ppSingerPkh           :: !Ledger.PaymentPubKeyHash,
          ppShowTime            :: !LedgerApiV2.POSIXTime,
          ppAmountDeposited     :: !P.Integer,
          ppShowId              :: !P.Integer
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
                
                OnChain.venue    = venuePkh dp,
                OnChain.manager  = managerPkh dp, 
                OnChain.bassPlayer  = bassPlayerPkh dp,
                OnChain.drummer  = drummerPkh dp,
                OnChain.guitarPlayer  = guitarPlayerPkh dp,
                OnChain.singer  = singerPkh dp,
                OnChain.showTime = showTime dp,
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

-- Refund
-- Business Rule: Venue can cancel contract BEFORE showDate
-- Must be signed by both Venue and Manager
-- Can only be called by Venue, venue will get it's money back

-- PayManager
-- Manager can request a payment for all funds
-- Must be AFTER showDate
-- Must be signed by both Venue and Manager
-- All money goes to manager
-- 20% of money pays to manager
-- 80% (or remainder after fees), creates utxos for each band member wallet address

-- PayBandMember
-- Band Member can request a payment
-- Must be AFTER showDate
-- Must be signed by both BandMember and Manager ?? (maybe just band member)
-- BandMember PKH must be in datum
-- Request filters uxtos to find matching uxto and consumes entire utxo

-- Payout
payout :: forall w s. PayoutParams -> PlutusContract.Contract w s T.Text ()
payout PayoutParams{..} = do
    requestor <- PlutusContract.ownFirstPaymentPubKeyHash
    now <- PlutusContract.currentTime
    -- utxos <- utxosAt scrAddress
    if now < ppShowTime
        then PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline not yet reached - Deadline: %s - Now: %s" (P.show ppShowTime) (P.show now)
        else do
            let param = OnChain.EscrowDetails {
                        OnChain.venue               = ppVenuePkh,
                        OnChain.manager             = requestor,
                        OnChain.bassPlayer          = ppBassPlayerPkh,
                        OnChain.drummer             = ppDrummerPkh,
                        OnChain.guitarPlayer        = ppGuitarPlayerPkh,
                        OnChain.singer              = ppSingerPkh,
                        OnChain.showTime            = ppShowTime,
                        OnChain.amountDeposited     = ppAmountDeposited
                }
                r = OnChain.Redeem { OnChain.redeem = ppShowId }
            maybeutxo <- findUtxoInValidator param ppShowId
            case maybeutxo of
                Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "NO MATCHING UTXOS FOUND - WRONG SHOW ID?"
                Just (oref, o) -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
                    let lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                                  Constraints.plutusV2OtherScript (OnChain.validator param)
                        tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                            Constraints.mustValidateIn (LedgerApiV2.from now) P.<>
                            Constraints.mustPayToPubKey requestor (getTotalValuePay o)
                    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.DepositType lookups tx
                    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
                    PlutusContract.logInfo @P.String $ "Payout complete"


-- payout :: forall w s. PayoutParams -> PlutusContract.Contract w s T.Text ()
-- payout PayoutParams{..} = do
--     requestor <- PlutusContract.ownFirstPaymentPubKeyHash
--     now <- PlutusContract.currentTime
--     if now < ppShowTime
--         then PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline not yet reached - Deadline: %s - Now: %s" (P.show ppShowTime) (P.show now)
--         else do
--             let param = OnChain.EscrowDetails {
--                         OnChain.venue               = ppVenuePkh,
--                         OnChain.manager             = requestor,
--                         OnChain.bassPlayer          = ppBassPlayerPkh,
--                         OnChain.drummer             = ppDrummerPkh,
--                         OnChain.guitarPlayer        = ppGuitarPlayerPkh,
--                         OnChain.singer              = ppSingerPkh,
--                         OnChain.showTime            = ppShowTime,
--                         OnChain.amountDeposited     = ppAmountDeposited
--                 }
--                 r = OnChain.Redeem { OnChain.redeem = ppShowId }
--             maybeutxo <- findUtxoInValidator param ppShowId
--             case maybeutxo of
--                 Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "NO MATCHING UTXOS FOUND - WRONG SHOW ID?"
--                 Just (oref, o) -> do
--                     PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
--                     let lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
--                                   Constraints.plutusV2OtherScript (OnChain.validator param)
--                         tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
--                             Constraints.mustValidateIn (LedgerApiV2.from now) P.<>
--                             Constraints.mustPayToPubKey requestor (getTotalValuePay o)
--                     submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.DepositType lookups tx
--                     Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
--                     PlutusContract.logInfo @P.String $ "Payout complete"

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
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "findUtxoInValidator: gparam is: %s" (P.show gparam)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "findUtxoInValidator: OnChain.address gparam is: %s" (P.show OnChain.address gparam)
    utxos <- PlutusContract.utxosAt $ OnChain.address gparam
    PlutusContract.logInfo @P.String $ TextPrintf.printf "findUtxoInValidator: utxos found are : %s" (P.show utxos)
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs n
    return out

getTotalValuePay :: LedgerTx.ChainIndexTxOut -> Ledger.Value
getTotalValuePay o = do
    Ada.toValue $ (Ada.fromValue $ LedgerTx._ciTxOutValue o) `Ada.divide` 20
    -- return tValue


-- validator :: Validator
-- validator = Scripts.validatorScript typedValidator

-- scrAddress :: Ledger.Address
-- scrAddress = scriptAddress validator

-- This puts all together. The select means to offer selection to the user. 
endpoints :: PlutusContract.Contract () GigSchema T.Text ()
endpoints = PlutusContract.awaitPromise (deposit' `PlutusContract.select` payout') >> endpoints
    where 
        deposit' = PlutusContract.endpoint @"Deposit" deposit
        payout' = PlutusContract.endpoint @"Payout" payout