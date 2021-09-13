{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- This solution has been contributed by George Flerovsky

module BlockfrostHello
  ( hello
  , helloSerialised
  , helloSBS
  , helloHash
  , Hello
  , helloInstance
  , helloValidator
  , helloMessage
  , helloAddr
  ) where

import           Prelude               hiding (($))

import           Cardano.Api.Shelley   (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import           Ledger                hiding (singleton)
import qualified Ledger.Typed.Scripts  as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude      as P hiding (Semigroup (..), unless)


{-
  The "hello world" message as a bytestring
-}

helloMessage :: P.BuiltinByteString
helloMessage = "Welcome to Alonzo Era! - Five Binaries team"

{-
   The Hello World validator script
-}

{-# INLINABLE hello #-}

hello :: P.BuiltinByteString -> P.BuiltinByteString -> P.BuiltinByteString -> ScriptContext -> P.Bool
hello keyword datum redeemer _context =
  P.traceIfFalse "yolo" (keyword P.== datum)

{-
    As a ScriptInstance
-}

data Hello
instance Scripts.ValidatorTypes Hello where
    type instance DatumType Hello = P.BuiltinByteString
    type instance RedeemerType Hello = P.BuiltinByteString

helloInstance :: Scripts.TypedValidator Hello
helloInstance = Scripts.mkTypedValidator @Hello
    ($$(PlutusTx.compile [|| hello ||]) `PlutusTx.applyCode` PlutusTx.liftCode helloMessage)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @P.BuiltinByteString @P.BuiltinByteString

{-
    As a Validator
-}

helloValidator :: Validator
helloValidator = Scripts.validatorScript helloInstance

-- hash

helloHash :: ValidatorHash
helloHash = Scripts.validatorHash helloInstance

{-
    As a Script
-}

helloScript :: Script
helloScript = unValidatorScript helloValidator

{-
    As a Short Byte String
-}

helloSBS :: SBS.ShortByteString
helloSBS =  SBS.toShort . LBS.toStrict $ serialise helloScript

{-
    As a Serialised Script
-}

helloSerialised :: PlutusScript PlutusScriptV1
helloSerialised = PlutusScriptSerialised helloSBS


helloAddr = scriptAddress helloValidator
