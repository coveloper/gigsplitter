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
        { recipientVenue    :: Ledger.PaymentPubKeyHash,
          recipientManager  :: Ledger.PaymentPubKeyHash, 
          recipientSinger   :: Ledger.PaymentPubKeyHash,
          recipientBass     :: Ledger.PaymentPubKeyHash, 
          recipientDrums    :: Ledger.PaymentPubKeyHash,
          recipientGuitar   :: Ledger.PaymentPubKeyHash,
          paymentDeadline   :: LedgerApiV2.POSIXTime,
          amountDeposited   :: P.Integer,
          showId            :: P.Integer
        }
    deriving (P.Show, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON)
PlutusTx.unstableMakeIsData ''DepositParams
-- PlutusTx.makeLift ''DepositParams

type GigSchema =
    PlutusContract.Endpoint "Deposit" DepositParams
    PlutusContract..\/ PlutusContract.Endpoint "Payout" DepositParams

-- deposit
deposit :: PlutusContract.AsContractError e => DepositParams -> PlutusContract.Contract w s e ()
deposit dp = do
    let p = OnChain.DepositDetails
            {
                -- OnChain.creator = recipientVenue dp,
                -- OnChain.beneficiary = recipientManager dp,
                -- OnChain.deadline = paymentDeadline dp
                OnChain.recipientVenue    = recipientVenue dp,
                OnChain.recipientManager  = recipientManager dp, 
                OnChain.recipientSinger   = recipientSinger dp,
                OnChain.recipientBass     = recipientBass dp, 
                OnChain.recipientDrums    = recipientDrums dp,
                OnChain.recipientGuitar   = recipientGuitar dp,
                OnChain.paymentDeadline   = paymentDeadline dp,
                OnChain.amountDeposited   = amountDeposited dp,
                OnChain.showId            = showId dp
            }
        d = OnChain.Dat { OnChain.ddata = showId dp}
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


-- payout
-- payout :: forall w s e. PlutusContract.AsContractError e => DepositParams -> PlutusContract.Contract w s e ()
payout :: forall w s. DepositParams -> PlutusContract.Contract w s T.Text ()
payout redeem = do
    PlutusContract.logInfo @P.String $ "Placeholder for Payout"
    -- if redeem == 300
        -- then do
                -- utxos <- PlutusContract.utxosAt OnChain.address
                -- let orefs   = fst <$> Map.toList utxos
                --     lookups = Constraints.unspentOutputs utxos      P.<>
                --             Constraints.plutusV2OtherScript OnChain.validator
                --     tx :: Constraints.TxConstraints Void.Void Void.Void
                --     tx      = mconcat [Constraints.mustSpendScriptOutput oref $ ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData (OnChain.Redeem redeem) | oref <- orefs]
                -- submittedTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
                -- Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
                -- PlutusContract.logInfo @P.String $ "collected gifts"
        -- else PlutusContract.logInfo @P.String $ "Wrong guess"
        
-- close
-- unlock :: Promise () GigSchema T.Text ()
-- unlock = endpoint @"unlock" 
-- (unlockFunds . mkSplitData)


-- This puts all together. The select means to offer selection to the user. 
endpoints :: PlutusContract.Contract () GigSchema T.Text T.Text
endpoints = PlutusContract.awaitPromise (deposit' `PlutusContract.select` payout') >> endpoints
    where 
        deposit' = PlutusContract.endpoint @"Deposit" deposit
        payout' = PlutusContract.endpoint @"Payout" payout

-- endpoints :: PlutusContract.Contract () GigSchema T.Text ()
-- -- BLOCK10

-- endpoints = PlutusContract.selectList [deposit, unlock]