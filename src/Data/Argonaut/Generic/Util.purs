module Data.Argonaut.Generic.Util where

import Prelude
import Data.Array (head, null, length)
import Data.Foldable (all)
import Data.Generic (GenericSignature(SigRecord), GenericSpine(SRecord), DataConstructor)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (lastIndexOf, drop, Pattern(..))

allConstructorsNullary :: Array DataConstructor -> Boolean
allConstructorsNullary = all (null <<< _.sigValues)


-- | Needed for applying unwrapUnaryRecords.
isUnaryRecord :: Array DataConstructor -> Boolean
isUnaryRecord constrSigns = length constrSigns == 1 -- Only one constructor


stripModulePath :: String -> String
stripModulePath constr = case lastIndexOf (Pattern ".") constr of
                    Nothing -> constr
                    Just i -> drop (i+1) constr

-- | Check whether a given spine is a record
spineIsRecord :: GenericSpine -> Boolean
spineIsRecord r = case r of
  SRecord _ -> true
  _ -> false

-- | Check whether a constructor is a Haskell constructor with record fields.
constructorIsRecord :: DataConstructor -> Boolean
constructorIsRecord constr = fromMaybe false $ do
  record <- (_ $ unit) <$> head constr.sigValues
  case record of
    SigRecord _ -> pure true
    _ -> pure false
