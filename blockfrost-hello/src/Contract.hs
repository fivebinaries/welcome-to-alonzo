{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Contract where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void, absurd)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Ada           as Ada
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Playground.Contract  (ToArgument, ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import           Prelude              (IO, Semigroup (..), Show (..), String)
import qualified Prelude
import           Text.Printf          (printf)

import           BlockfrostHello

import           Control.Lens
import           Data.Default
import           Data.String          (IsString (..))

type HelloSchema =
       Endpoint "sayhello" ()
       .\/ Endpoint "saysomething" String

sayhello :: AsContractError e => Promise w HelloSchema e ()
sayhello = endpoint @"sayhello" $ \_ -> do
    logInfo @String $ printf "fhash %s" (show helloHash)

    pk <- ownPubKey
    -- first we lock..
    let
        lookups' =  Constraints.typedValidatorLookups helloInstance
        --tx' :: TxConstraints () BuiltinByteString
        tx'      = Constraints.mustPayToTheScript (helloMessage) (Ada.lovelaceValueOf 10)
    lockTx <- submitTxConstraintsWith @Hello lookups' tx'
    void $ awaitTxConfirmed $ txId lockTx

    utxos <- utxosAt helloAddr
    logInfo @String $ printf "utxos %s" (show utxos)

    let orefs   = fst <$> Map.toList utxos

        lookups = Constraints.unspentOutputs utxos
                   <> Constraints.typedValidatorLookups helloInstance
                   <> Constraints.otherScript helloValidator

        tx =  mempty
            <> Prelude.mconcat [
             Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ("" :: BuiltinByteString))
            | oref <- orefs ]

    ledgerTx <- submitTxConstraintsWith @Hello lookups tx
    logInfo @String $ printf "ltx %s" (show ledgerTx)
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "success!"

saysomething :: AsContractError e => Promise w HelloSchema e ()
saysomething = endpoint @"saysomething" $ \something -> do
    logInfo @String $ printf "fhash %s" (show helloHash)

    pk <- ownPubKey
    -- first we lock..
    let
        lookups' =  Constraints.typedValidatorLookups helloInstance
        tx'      = Constraints.mustPayToTheScript (fromString something) (Ada.lovelaceValueOf 10)
    lockTx <- submitTxConstraintsWith @Hello lookups' tx'
    void $ awaitTxConfirmed $ txId lockTx



{--
    utxos <- utxosAt faddr
    logInfo @String $ printf "faddr %s" (show faddr)
    -- faddr Address {addressCredential = ScriptCredential bf2a9c89a7323439d1eff913c0d93dd2a4e9a4040a03a18b8a16bb4c, addressStakingCredential = Nothing}
    logInfo @String $ printf "utxos %s" (show utxos)
    -- utxos fromList [(TxOutRef {txOutRefId = 8ddae187b3d0e821cd2597482f1f13ba644151c48ed90fc891a948799eb64435, txOutRefIdx = 1},ScriptChainIndexTxOut {_ciTxOutAddress = Address {addressCredential = ScriptCredential bf2a9c89a7323439d1eff913c0d93dd2a4e9a4040a03a18b8a16bb4c, addressStakingCredential = Nothing}, _ciTxOutValidator = Left bf2a9c89a7323439d1eff913c0d93dd2a4e9a4040a03a18b8a16bb4c, _ciTxOutDatum =  Right (Datum {getDatum = Constr 0 [B "5\222\221)\130\160<\243\158}\206\ETX\200\&9\153O\253\236.\198\176O\FS\242\212\SOa\163"]}), _ciTxOutValue = Value (Map [(,Map [("",66)])])})]
    let
        utxos'  = Map.filter (\x -> (_ciTxOutDatum x) == Right (Datum $ PlutusTx.toBuiltinData lu)) utxos
    let orefs   = fst <$> Map.toList utxos'

    -- then we mint using utxo
    let val     = Value.singleton (stagedCurrency)
                      (mpTokenName stagedPolicyParams)
                      grabAmount
                      -- (mpMaxAmount stagedPolicyParams)
        lookups =  Prelude.mconcat [
           Constraints.unspentOutputs utxos'
          , Constraints.otherScript (Scripts.validatorScript fixedFaucetValidator)
          -- "ConstraintResolutionError (ValidatorHashNotFound bf2a9c89a7323439d1eff913c0d93dd2a4e9a4040a03a18b8a16bb4c)"
--          , Constraints.otherScript validator
--         ,  Constraints.otherScript stagedPolicy
         -- , Constraints.mintingPolicy stagedPolicy
          , Constraints.mintingPolicy (Scripts.forwardingMintingPolicy fixedFaucetValidator)
          , Constraints.typedValidatorLookups fixedFaucetValidator
          ]
        --tx :: TxConstraints (Scripts.RedeemerType ()) (Scripts.DatumType ())
        --tx :: TxConstraints (Scripts.RedeemerType ()) (())
        tx :: TxConstraints () FaucetDatum
        tx      =
            Constraints.mustMintValue val
          <> Constraints.mustPayToTheScript lu (Ada.lovelaceValueOf grabAmount)
            -- XX: same as
--            Constraints.mustMintCurrency vhash
--                      (mpTokenName stagedPolicyParams)
--                      66
            <> Prelude.mconcat [
             Constraints.mustSpendScriptOutput oref unitRedeemer
            | oref <- orefs ]
--          , Constraints.mustPayToTheScript @Void () (Ada.lovelaceValueOf grabAmount)
--          ,
--            , Constraints.mustPayToTheScript () (Ada.lovelaceValueOf grabAmount)
--            , Constraints.mustPayToOtherScript fhash unitDatum (Ada.lovelaceValueOf grabAmount)
--            , Constraints.mustPayToOtherScript valHash unitDatum (Ada.lovelaceValueOf grabAmount)
--          , Constraints.mustPayToPubKey (scriptHashAddress validator)
--          ]
--    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    ledgerTx <- submitTxConstraintsWith @Faucet lookups tx
    logInfo @String $ printf "ltx %s" (show ledgerTx)
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "forged %s" (show val)

    mutxos <- utxosAt (pubKeyHashAddress (pubKeyHash pk))
    logInfo @String $ printf "myutxos %s" (show mutxos)
    --}

endpoints :: Contract () HelloSchema Text ()
endpoints = forever $ selectList [ sayhello ]

mkSchemaDefinitions ''HelloSchema

--mkKnownCurrencies []
