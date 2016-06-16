-- | Flexible generic decoding. Use this for defining your own custom encodings
-- | or use "Data.Argonaut.Generic.Aeson" and "Data.Argonaut.Generic.Argonaut" for concrete codecs.
module Data.Argonaut.Generic.Decode
  (
    genericDecodeJson
  , genericDecodeJson'
  , genericUserDecodeJson'
  , decodeMaybe
  , module Data.Argonaut.Generic.Options
  , mFail
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Data.Argonaut.Core (Json(), isNull, foldJsonNull, foldJsonBoolean, foldJsonNumber, foldJsonString, toArray, toNumber, toObject, toString, toBoolean)
import Data.Argonaut.Generic.Options
import Data.Array (zipWithA, length)
import Data.Either (either, Either(..))
import Data.Foldable (find)
import Data.Generic (class Generic, GenericSpine(..), GenericSignature(..), DataConstructor(), fromSpine, toSignature)
import Data.Int (fromNumber)
import Data.List (List(..), fromFoldable)
import Data.Map as Map
import Data.Maybe (maybe, Maybe(..), fromMaybe)
import Data.String (charAt, toChar)
import Data.StrMap as M
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Data.Array.Partial as Unsafe


-- | Decode `Json` representation of a value which has a `Generic` type.
genericDecodeJson :: forall a. (Generic a) => Options -> Json -> Either String a
genericDecodeJson opts json = maybe (Left "fromSpine failed") Right <<< fromSpine
                =<< genericUserDecodeJson' opts (toSignature (Proxy :: Proxy a)) json


-- | Generically decode json, using a supplied userEncoding, falling back to genericEncodeJson':
genericUserDecodeJson' :: Options -> GenericSignature -> Json -> Either String GenericSpine
genericUserDecodeJson' opts'@(Options opts) sign json = fromMaybe (genericDecodeJson' opts' sign json)
                                                        (opts.userDecoding opts' sign json)


-- | Decode `Json` representation of a `GenericSpine`.
genericDecodeJson' :: Options -> GenericSignature -> Json -> Either String GenericSpine
genericDecodeJson' opts signature json = case signature of
 SigNumber -> SNumber <$> mFail "Expected a number" (toNumber json)
 SigInt -> SInt <$> mFail "Expected an integer number" (fromNumber =<< toNumber json)
 SigString -> SString <$> mFail "Expected a string" (toString json)
 SigChar -> SChar <$> mFail "Expected a char" (toChar =<< toString json)
 SigBoolean -> SBoolean <$> mFail "Expected a boolean" (toBoolean json)
 SigUnit -> pure SUnit
 SigArray thunk -> do
   jArr <- mFail "Expected an array" $ toArray json
   SArray <$> traverse (map const <<< genericUserDecodeJson' opts (thunk unit)) jArr
 SigRecord props -> do
   jObj <- mFail "Expected an object" $ toObject json
   SRecord <$> for props \({recLabel: lbl, recValue: val}) -> do
     pf <- mFail ("'" <> lbl <> "' property missing") (M.lookup lbl jObj)
     sp <- genericUserDecodeJson' opts (val unit) pf
     pure { recLabel: lbl, recValue: const sp }
 SigProd typeConstr constrSigns -> genericDecodeProdJson' opts typeConstr constrSigns json

genericDecodeProdJson' :: Options ->  String -> Array DataConstructor -> Json -> Either String GenericSpine
genericDecodeProdJson' opts'@(Options opts) tname constrSigns json =
  if opts.unwrapUnaryRecords && isUnaryRecord constrSigns
  then do
    let constr = Unsafe.head constrSigns
    let unwrapped = Unsafe.head constr.sigValues unit
    r <- genericUserDecodeJson' opts' unwrapped json
    pure (SProd constr.sigConstructor [const r])
  else
    if opts.allNullaryToStringTag && allConstructorsNullary constrSigns
    then decodeFromString
    else decodeTagged
  where
    decodeFromString = do
      tag <- mFail (decodingErr "Constructor name as string expected") (toString json)
      foundConstr <- findConstrFail tag
      pure (SProd foundConstr.sigConstructor [])
    decodeTagged = do
      jObj <- mFail (decodingErr "expected an object") (toObject json)
      tagJson  <- mFail (decodingErr "'" <> tagL <> "' property is missing") (M.lookup tagL jObj)
      tag <- mFail (decodingErr "'" <> tagL <> "' property is not a string") (toString tagJson)
      foundConstr <-  findConstrFail tag
      jVals <- mFail (decodingErr "'" <> contL <> "' property is missing") (M.lookup contL jObj)
      vals <- if opts.flattenContentsArray && (length foundConstr.sigValues == 1)
              then pure [jVals]
              else mFail (decodingErr "Expected array") (toArray jVals)
      sps  <- zipWithA (\k -> genericUserDecodeJson' opts' (k unit)) foundConstr.sigValues vals
      pure (SProd foundConstr.sigConstructor (const <$> sps))

    decodingErr msg = "When decoding a " <> tname <> ": " <> msg
    fixConstr      = opts.constructorTagModifier
    sumConf = case opts.sumEncoding of
      TaggedObject conf -> conf
      _ -> unsafeCrashWith "Only TaggedObject encoding is supported - FIX ME!"
    tagL = sumConf.tagFieldName
    contL = sumConf.contentsFieldName
    findConstrFail tag = mFail (decodingErr ("'" <> tag <> "' isn't a valid constructor")) (findConstr tag)
    findConstr tag = find ((tag == _) <<< fixConstr <<< _.sigConstructor) constrSigns


decodeMaybe :: forall a. (DecodeJson a) => Json -> Maybe a
decodeMaybe json = either (const Nothing) pure $ decodeJson json

mFail :: forall a. String -> Maybe a -> Either String a
mFail msg = maybe (Left msg) Right
