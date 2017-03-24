-- | Options for generic encoding/decoding.
--
--   By the means of `userEncoding`  and `userDecoding` you can provide custom rules
--   for certain data types instead of using the default generic encoding.
module Data.Argonaut.Generic.Options where


import Data.Argonaut.Core (Json())
import Data.Generic (GenericSpine, GenericSignature)
import Data.Either (Either)
import Data.Maybe (Maybe(..))


newtype Options = Options { -- newtype necessary to avoid: https://github.com/purescript/purescript/wiki/Error-Code-CycleInTypeSynonym
  -- | Modify the tag, e.g. strip module path with: `stripModulePath`e
  constructorTagModifier  :: String -> String
  -- | If all constructors of a sum type are nullary, just serialize the constructor name as a string.
, allNullaryToStringTag   :: Boolean
  -- | Options on how to do encoding of sum types.
, sumEncoding             :: SumEncoding
  -- | If a constructor has exactly one field, do not serialize as array.
  -- | Also, if the constructor has no fields, skip the contents field (aeson >= 1 behavior).
, flattenContentsArray    :: Boolean -- Flatten array to simple value, if constructor only takes a single value
  -- | You need a newtype wrapper encoding/decoding of records, set this
  --   to false if you want the plain Javascript object without a wrapping tagged object.
  --
  --   If an ADT only defines one data constructor, the data constructor will be omitted in the encoding when set to false.
  , encodeSingleConstructors :: Boolean
-- | You can choose to encode some data types differently than the generic default.
-- | Find a thorough example for doing this in "Data.Argonaut.Generic.Aeson"!
-- | Just return Nothing if you want to relay to generic encoding.
, userEncoding :: Options -> GenericSignature -> GenericSpine -> Maybe Json
-- | You can choose to decode some data types differently than the generic default.
-- | Just return Nothing, to relay to generic decoding.
, userDecoding :: Options -> GenericSignature -> Json -> Maybe (Either String GenericSpine)
  -- | Modify a fields label, e.g. convert the case from that used remotely.
  , fieldLabelModifier :: String -> String
  -- | If True record fields with a Nothing value will be omitted from the resulting object.
  -- | If False the resulting object will include those fields.
  , omitNothingFields :: Boolean
}

data SumEncoding =
  -- | Serialize as tagged object.
  -- | The Javascript object will have a tag field, with the
  -- | `constructorTagModifier constructorName` name as contents
  -- | and a contents field, which contains an array with the constructor
  -- | parameters.
  TaggedObject {
    tagFieldName :: String
  , contentsFieldName :: String
    -- | Should a contained record's fields be put in the tagged object record directly, like in Aeson?
    -- | http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#v:SumEncoding
  , unpackRecords :: Boolean
  }


-- | Use this for `userEncoding` if you don't want any special rules.
dummyUserEncoding :: Options -> GenericSignature -> GenericSpine -> Maybe Json
dummyUserEncoding _ _ _ = Nothing

-- | Use this for `userDecodeing` if you don't want any special rules.
dummyUserDecoding :: Options -> GenericSignature -> Json -> Maybe (Either String GenericSpine)
dummyUserDecoding _ _ _ = Nothing
