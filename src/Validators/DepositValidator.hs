{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Validators.DepositValidator where

import PlutusTx
import PlutusTx.Prelude
import qualified Ledger
import qualified Plutus.V2.Ledger.Api as V2LedgerApi
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts

--3 Onchain code

--Actual validator logic
depositV :: BuiltinData -> BuiltinData -> BuiltinData -> ()
depositV _ _ _ = error ()

--Boilerplate
depositVCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
depositVCompiled = $$(compile [|| depositV ||])

depositValidator :: V2UtilsScripts.Validator
depositValidator = V2LedgerApi.mkValidatorScript depositVCompiled

validatorHash :: Ledger.ValidatorHash
validatorHash = Ledger.validatorHash depositValidator

address :: Ledger.Address
address = Ledger.scriptHashAddress validatorHash