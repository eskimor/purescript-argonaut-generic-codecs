module Data.Argonaut.Generic.Util where

import Prelude

import Data.Foldable (all)
import Data.String (lastIndexOf, drop)
import Data.Array (null, length)
import Data.Generic (DataConstructor)
import Data.Maybe (Maybe(..))

allConstructorsNullary :: Array DataConstructor -> Boolean
allConstructorsNullary = all (null <<< _.sigValues)


-- | Needed for applying unwrapUnaryRecords.
isUnaryRecord :: Array DataConstructor -> Boolean
isUnaryRecord constrSigns = length constrSigns == 1 -- Only one constructor
                            

stripModulePath :: String -> String
stripModulePath constr = case lastIndexOf "." constr of
                    Nothing -> constr
                    Just i -> drop (i+1) constr
