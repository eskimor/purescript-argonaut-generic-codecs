-- | Flexible generic encoding. Use this for defining your own custom encodings
-- | or use "Data.Argonaut.Generic.Aeson" and "Data.Argonaut.Generic.Argonaut" for concrete codecs.
module Data.Argonaut.Generic.Encode
  (
    genericEncodeJson
  , genericEncodeJson'
  , genericUserEncodeJson'
  , module Data.Argonaut.Generic.Options
  ) where

import Prelude
import Data.Array.Partial as Unsafe
import Data.StrMap as SM
import Control.MonadPlus (guard)
import Data.Argonaut.Core (toObject, JObject, fromString, fromArray, Json, jsonNull, fromBoolean, fromNumber, fromObject)
import Data.Argonaut.Generic.Options (Options(..), SumEncoding(..), dummyUserDecoding, dummyUserEncoding)
import Data.Argonaut.Generic.Util (allConstructorsNullary, isUnaryRecord, spineIsRecord)
import Data.Array (head, length, concatMap, filter, zip, zipWith)
import Data.Foldable (foldr)
import Data.Generic (class Generic, GenericSpine(..), toSpine, GenericSignature(..), DataConstructor, toSignature)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.String (singleton)
import Data.Tuple (uncurry)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Type.Proxy (Proxy(..))


genericEncodeJson :: forall a. (Generic a) => Options -> a -> Json
genericEncodeJson opts = genericUserEncodeJson' opts sign <<< toSpine
  where sign = toSignature (Proxy :: Proxy a)


-- | Generically encode to json, using a supplied `userEncoding`, falling back to `genericEncodeJson'`:
genericUserEncodeJson' :: Options -> GenericSignature -> GenericSpine -> Json
genericUserEncodeJson' opts'@(Options opts) sign spine = fromMaybe (genericEncodeJson' opts' sign spine)
                                                        (opts.userEncoding opts' sign spine)

-- | Encode `GenericSpine` into `Json`.
-- | This function is mutually recursive with `genericUserEncodeJson'`, as for all descendent spines
-- | `genericUserEncodeJson'` is invoked.
genericEncodeJson' :: Options -> GenericSignature -> GenericSpine -> Json
genericEncodeJson' opts sign spine = case spine of
 SInt x            -> fromNumber $ toNumber x
 SString x         -> fromString x
 SChar x           -> fromString $ singleton x
 SNumber x         -> fromNumber x
 SBoolean x        -> fromBoolean x
 SUnit             -> jsonNull
 SArray thunks     -> case sign of
                        SigArray elemSign -> fromArray (genericUserEncodeJson' opts (elemSign unit) <<< (unit # _) <$> thunks)
                        _ -> unsafeCrashWith "Signature does not match value, please don't do that!"
 SProd constr args -> case sign of
                        SigProd _ constrSigns -> genericEncodeProdJson' opts constrSigns constr args
                        _ -> unsafeCrashWith "Signature does not match value, please don't do that!"
 SRecord fields    -> case sign of
                        SigRecord sigs -> genericEncodeRecordJson' opts sigs fields
                        _ -> unsafeCrashWith "Signature does not match value, please don't do that!"

genericEncodeRecordJson' :: Options
                        -> Array { recLabel :: String, recValue :: Unit -> GenericSignature }
                        -> Array { recLabel :: String, recValue :: Unit -> GenericSpine }
                        -> Json
genericEncodeRecordJson' opts sigs fields = fromObject <<< foldr (uncurry addField) SM.empty $ zip sigs fields
  where
    addField sig field = SM.insert field.recLabel (genericUserEncodeJson' opts (sig.recValue unit) (field.recValue unit))

genericEncodeProdJson' :: Options -> Array DataConstructor -> String -> Array (Unit -> GenericSpine) -> Json
genericEncodeProdJson' opts'@(Options opts) constrSigns constr args =
  if not opts.encodeSingleConstructors && isUnaryRecord constrSigns
  then contents
  else
    if opts.allNullaryToStringTag && allConstructorsNullary constrSigns
    then fromString fixedConstr
    else case containedRecord of
      Nothing  -> fromObject
                  $ SM.insert sumConf.tagFieldName (fromString fixedConstr)
                  $ if opts.flattenContentsArray && length args == 0
                    then SM.empty
                    else SM.singleton sumConf.contentsFieldName contents
      Just obj -> fromObject
                  $ SM.insert sumConf.tagFieldName (fromString fixedConstr) obj
  where
    sumConf            = case opts.sumEncoding of
                          TaggedObject conf -> conf
    fixedConstr        = opts.constructorTagModifier constr
    encodedArgs        = genericEncodeProdArgs opts' constrSigns constr args
    containedRecord :: Maybe JObject
    containedRecord    = do -- handle unpackRecord
                           guard sumConf.unpackRecords
                           arg <- (_ $ unit ) <$> head args
                           guard $ spineIsRecord arg
                           toObject contents

    contents           = if (opts.flattenContentsArray || sumConf.unpackRecords) && length encodedArgs == 1
                         then unsafeHead encodedArgs
                         else fromArray encodedArgs



genericEncodeProdArgs :: Options -> Array DataConstructor -> String -> Array (Unit -> GenericSpine) -> Array Json
genericEncodeProdArgs opts constrSigns constr args = zipWith (genericUserEncodeJson' opts) sigValues values
  where
   lSigValues = concatMap (\c -> c.sigValues)
                   <<< filter (\c -> c.sigConstructor == constr) $ constrSigns
   sigValues = (unit # _) <$> lSigValues
   values = (unit # _) <$> args

unsafeHead :: forall a. Array a -> a
unsafeHead = unsafePartial Unsafe.head
