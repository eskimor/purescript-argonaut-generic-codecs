-- | Straight forward encoding/decoding with no special rules.
module Data.Argonaut.Generic.Argonaut where


import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject, JArray, jsonNull, isNull, toObject, toArray)
import Data.Argonaut.Options
import Data.Argonaut.Generic.Encode
import Data.Argonaut.Generic.Decode hiding (decodeMaybe)
import Data.Either (Either(), either)
import Data.Foldable (foldr)
import Data.Generic (Generic, GenericSpine(..), toSpine, GenericSignature(..), DataConstructor(), toSignature)
import Data.Int (toNumber)
import Data.List (List(..), fromList)
import Data.List as L
import Data.Map as M
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromChar)
import Data.StrMap as SM
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Data.Tuple (uncurry)
import Data.Array (length, concatMap, filter, zip, zipWith)
import Data.Array.Unsafe as Unsafe
import Partial.Unsafe (unsafeCrashWith)


-- | Default for straight forward argonaut encoding.
options :: Options
options = Options {
  constructorTagModifier : id
, allNullaryToStringTag  : false
, sumEncoding            : argonautSumEncoding
, flattenContentsArray   : false
, unwrapUnaryRecords     : false
, userEncoding           : dummyUserEncoding
, userDecoding           : dummyUserDecoding
}

sumEncoding :: SumEncoding
sumEncoding = TaggedObject {
  tagFieldName           : "tag"
, contentsFieldName      : "values"
}

-- | Encode any `Generic` data structure into `Json`,
-- | formatted according to argonautOptions
encodeJson :: forall a. (Generic a) => a -> Json
encodeJson = genericEncodeJson options

-- | Decode `Json` representation of a value which has a `Generic` type
-- | with Argonaut options.
decodeJson :: forall a. (Generic a) => Json -> Either String a
decodeJson = genericDecodeJson argonautOptions
