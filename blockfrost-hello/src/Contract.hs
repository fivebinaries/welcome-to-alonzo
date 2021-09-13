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

endpoints :: Contract () HelloSchema Text ()
endpoints = forever $ selectList [ sayhello ]

mkSchemaDefinitions ''HelloSchema
