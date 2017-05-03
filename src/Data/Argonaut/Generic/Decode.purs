-- | Flexible generic decoding. Use this for defining your own custom encodings
-- | or use "Data.Argonaut.Generic.Aeson" and "Data.Argonaut.Generic.Argonaut" for concrete codecs.
module Data.Argonaut.Generic.Decode
  (
    genericDecodeJson
  , genericDecodeJson'
  , genericUserDecodeJson'
  , module Data.Argonaut.Generic.Options
  , mFail
  ) where

import Data.Argonaut.Generic.Util
import Data.Array.Partial as Unsafe
import Data.StrMap as M
import Data.Argonaut.Core (Json, fromArray, fromObject, jsonNull, toArray,
                          toBoolean, toNumber, toObject, toString, stringify)
import Data.Argonaut.Generic.Options (Options(..), SumEncoding(..), dummyUserDecoding, dummyUserEncoding)
import Data.Array (zipWithA, length)
import Data.Either (Either(Right, Left))
import Data.Foldable (find)
import Data.Generic (class Generic, DataConstructor, GenericSignature(..), GenericSpine(..), fromSpine, toSignature)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (toChar)
import Data.Traversable (traverse, for)
import Partial.Unsafe (unsafePartial)
import Prelude (not, const, pure, bind, unit, map, (&&), ($), (<<<), (==), (<>), (<$>), (=<<), (||))
import Type.Proxy (Proxy(..))


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
genericDecodeJson' opts'@(Options opts) signature json = case signature of
  SigNumber -> SNumber <$> mFail' "Expected a number" json (toNumber json)
  SigInt -> SInt <$> mFail' "Expected an integer number" json (fromNumber =<< toNumber json)
  SigString -> SString <$> mFail' "Expected a string" json (toString json)
  SigChar -> SChar <$> mFail' "Expected a char" json (toChar =<< toString json)
  SigBoolean -> SBoolean <$> mFail' "Expected a boolean" json (toBoolean json)
  SigUnit -> pure SUnit
  SigArray thunk -> do
    jArr <- mFail' "Expected an array" json $ toArray json
    SArray <$> traverse (map const <<< genericUserDecodeJson' opts' (thunk unit)) jArr
  SigRecord props -> do
    jObj <- mFail' "Expected an object" json $ toObject json
    SRecord <$> for props \({recLabel: lbl, recValue: val}) -> do
      let jLabel = (opts.fieldLabelModifier lbl)
      let propSig = (val unit)
      pf <- if (sigIsMaybe propSig) && opts.omitNothingFields
            then maybe (Right jsonNull) Right $ M.lookup jLabel jObj
            else mFail' ("'" <> jLabel <> "' property missing") json (M.lookup jLabel jObj)
      sp <- genericUserDecodeJson' opts' propSig pf
      pure { recLabel: lbl, recValue: const sp }
  SigProd typeConstr constrSigns -> genericDecodeProdJson' opts' typeConstr constrSigns json

genericDecodeProdJson' :: Options ->  String -> Array DataConstructor -> Json -> Either String GenericSpine
genericDecodeProdJson' opts'@(Options opts@{ sumEncoding: TaggedObject sumConf }) tname constrSigns json = unsafePartial $
    -- ^ Only TaggedObject encoding is supported - FIX ME!
  if not opts.encodeSingleConstructors && isUnaryRecord constrSigns
  then do
    let constr = Unsafe.head constrSigns
    decodeConstructor constr json
  else
    if opts.allNullaryToStringTag && allConstructorsNullary constrSigns
    then decodeFromString
    else decodeTagged
  where
    decodeFromString = do
      tag <- mFail' (decodingErr "Constructor name as string expected") json (toString json)
      foundConstr <- findConstrFail tag
      pure (SProd foundConstr.sigConstructor [])
    decodeTagged = do
      jObj <- mFail' (decodingErr "expected an object") json (toObject json)
      tagJson  <- mFail' (decodingErr "'" <> tagL <> "' property is missing") json (M.lookup tagL jObj)
      tag <- mFail' (decodingErr "'" <> tagL <> "' property is not a string") json (toString tagJson)
      foundConstr <-  findConstrFail tag
      jVals <- if sumConf.unpackRecords && constructorIsRecord foundConstr
               then pure $ fromObject $ M.delete tagL jObj -- Just use the object we already have
               else case M.lookup contL jObj of
                      Nothing -> if opts.flattenContentsArray
                                 then Right $ fromArray []
                                 else Left (decodingErr "'" <> contL <> "' property is missing")
                      Just jVals -> Right jVals
      decodeConstructor foundConstr jVals

    decodeConstructor constr jVals = do
      vals <- if (opts.flattenContentsArray || sumConf.unpackRecords) && (length constr.sigValues == 1)
              then pure [jVals]
              else mFail' (decodingErr "Expected array") json (toArray jVals)
      sps <- zipWithA (\k -> genericUserDecodeJson' opts' (k unit)) constr.sigValues vals
      pure (SProd constr.sigConstructor (const <$> sps))

    decodingErr msg = "When decoding a " <> tname <> ": " <> msg
    fixConstr      = opts.constructorTagModifier
    tagL = sumConf.tagFieldName
    contL = sumConf.contentsFieldName
    findConstrFail tag = mFail (decodingErr ("'" <> tag <> "' isn't a valid constructor")) (findConstr tag)
    findConstr tag = find ((tag == _) <<< fixConstr <<< _.sigConstructor) constrSigns



mFail :: forall a. String -> Maybe a -> Either String a
mFail msg = maybe (Left msg) Right

mFail' :: forall a. String -> Json -> Maybe a -> Either String a
mFail' msg json = mFail (msg <> ": '" <> stringify json <> "'")
