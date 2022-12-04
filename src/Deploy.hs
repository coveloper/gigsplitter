{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Deploy where
 
import           Cardano.Api       
import           Cardano.Api.Shelley                 (PlutusScript (..))
import           Codec.Serialise                    (serialise)
import qualified Data.Aeson                          as DataAeson
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Short               as SBS
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import qualified Ledger

import qualified GigsplitterOnChain                as OnChain

deploy :: IO()
deploy = do
    writeInitDatum
    --writeDepositDatum
    --_ <- writeMyFirstValidatorScript



    return ()

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeInitDatum :: IO ()
writeInitDatum = writeJSON "../scripts/plutus-scripts/unit.json" ()

-- writeDepositDatum :: IO ()
-- writeDepositDatum = 
--     let contributor = OnChain.Dat
--             {
--                 OnChain.ddata = 89554122355
--             }
--         d = PlutusTx.toBuiltinData contributor
--     in writeJSON "../plutus-scripts/parameterized-datum.json" d

writeGigsplitterValidatorScript :: IO (Either (FileError ()) ())
writeGigsplitterValidatorScript = writeValidator "../scripts/plutus-scripts/GigsplitterValidator.plutus" $ OnChain.validator $ OnChain.EscrowDetails 
    {
        OnChain.venue             = Ledger.PaymentPubKeyHash "87aaedbffe58187afa347dbdbad230e162f67a32e1e9c4e0275b6deb",
        OnChain.manager           = Ledger.PaymentPubKeyHash "80e627c7265c7c8e0977351cc1380ab3a1abf8fd16087fc0d4feb722", 
        OnChain.bassPlayer        = Ledger.PaymentPubKeyHash "5161d346e754260809ac9516333f3cb758610fe75e0fad3febd68509",
        OnChain.drummer           = Ledger.PaymentPubKeyHash "56d6b2d5d10b8cdf2a80d65f507808f1a13cebf8cbaaa2c90c0a033c",
        OnChain.guitarPlayer      = Ledger.PaymentPubKeyHash "3151b18f1dd6564c25977c9a324e52c325dd46ed6dbf2b9e025538ad",
        OnChain.singer            = Ledger.PaymentPubKeyHash "fc45b09323003a6ad482ac9c944b5d8a6e2a84c0872d5f642be72e71",
        OnChain.showTime          = 1668463801,
        OnChain.amountDeposited   = 20000000
    }