-- | Straight forward encoding/decoding with no special rules.
module Data.Argonaut.Generic.Argonaut where


import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Generic.Options (Options(..), SumEncoding(..), dummyUserDecoding, dummyUserEncoding)
import Data.Argonaut.Generic.Encode (genericEncodeJson)
import Data.Argonaut.Generic.Decode (genericDecodeJson)
import Data.Generic (class Generic)
import Data.Either (Either)


-- | Default for straight forward argonaut encoding.
options :: Options
options = Options {
  constructorTagModifier   : id
, allNullaryToStringTag    : false
, sumEncoding              : sumEncoding
, flattenContentsArray     : false
, encodeSingleConstructors : true
, userEncoding             : dummyUserEncoding
, userDecoding             : dummyUserDecoding
, fieldLabelModifier       : id
, omitNothingFields        : false
}

sumEncoding :: SumEncoding
sumEncoding = TaggedObject {
  tagFieldName             : "tag"
, contentsFieldName        : "values"
, unpackRecords            : false
}

-- | Encode any `Generic` data structure into `Json`,
-- | formatted according to argonautOptions
encodeJson :: forall a. (Generic a) => a -> Json
encodeJson = genericEncodeJson options

-- | Decode `Json` representation of a value which has a `Generic` type
-- | with Argonaut options.
decodeJson :: forall a. (Generic a) => Json -> Either String a
decodeJson = genericDecodeJson options
