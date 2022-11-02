validateSplit :: SplitData -> () -> ScriptContext -> Bool
validateSplit SplitData{recipient1, recipient2, amount} _ ScriptContext{scriptContextTxInfo} =
    let half = Ada.divide amount 2 in
    Ada.fromValue (valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash recipient1)) >= half &&
    Ada.fromValue (valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash recipient2)) >= (amount - half)

-- BLOCK3

data Split
instance Scripts.ValidatorTypes Split where
    type instance RedeemerType Split = ()
    type instance DatumType Split = SplitData

splitValidator :: Scripts.TypedValidator Split
splitValidator = Scripts.mkTypedValidator @Split
    $$(PlutusTx.compile [|| validateSplit ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @SplitData @()