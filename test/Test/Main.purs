module Test.Main where

import Prelude

import Data.Argonaut.Core hiding (toNumber)
import Data.Argonaut.Generic.Options
import Data.Argonaut.Generic.Encode
import Data.Argonaut.Generic.Decode
import Data.Argonaut.Generic.Aeson as Aeson
import Data.Argonaut.Generic.Argonaut as Argonaut
import Data.Either
import Data.Int (toNumber)
import Data.Tuple
import Data.Maybe
import Data.Array hiding ((:))
import Data.Generic
import Data.Foldable (foldl)
import Data.List (fromFoldable, List(..), (:))
import Data.StrMap as SM

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Console
import Data.StrMap as M

import Test.Assert (assert', ASSERT)
import Test.StrongCheck
import Test.StrongCheck.Gen
import Test.StrongCheck.Generic


newtype MyRecord = MyRecord { foo :: String, bar :: Int}
derive instance genericMyRecord :: Generic MyRecord

data User = Anonymous
          | Guest String
          | Registered { name :: String
                       , age :: Int
                       , balance :: Number
                       , banned :: Boolean
                       , tweets :: Array String
                       , followers :: Array User
                       }
          | SmallRecord { foo :: String
                        , bar :: Int
                        }
derive instance genericUser :: Generic User


data AllNullary = Nullary1 | Nullary2 | Nullary3
derive instance genericAllNullary :: Generic AllNullary
instance genericEqAllNullary :: Eq AllNullary where
  eq = gEq

data MultipleArgs = MArgs Int Int String | NArgs
derive instance genericMultipleArgs :: Generic MultipleArgs
instance genericEqMArgs :: Eq MultipleArgs where
  eq = gEq

newtype NewTypeWrapper1 = NewTypeWrapper1 { test :: String }
derive instance genericNewTypeWrapper1 :: Generic NewTypeWrapper1
instance eqNewTypeWrapper1 :: Eq NewTypeWrapper1 where
  eq = gEq

data NewTypeWrapper2 = NewTypeWrapper2 {test :: Int}
derive instance genericNewTypeWrapper2 :: Generic NewTypeWrapper2
instance eqNewTypeWrapper2 :: Eq NewTypeWrapper2 where
  eq = gEq

data UnwrapTestMult = UnwrapTestMult Int String
derive instance genericUnwrapTestMult :: Generic UnwrapTestMult
instance eqUnwrapTestMult :: Eq UnwrapTestMult where
  eq = gEq

data UnwrapTestSingle = UnwrapTestSingle Int
derive instance genericUnwrapTestSingle :: Generic UnwrapTestSingle
instance eqUnwrapTestSingle :: Eq UnwrapTestSingle where
  eq = gEq

prop_iso_generic :: Options -> GenericValue -> Result
prop_iso_generic opts genericValue =
    annotate check $ "prop_iso_generic did not hold for: " <> unsafeToString genericValue
  where
    check = Right val.spine == genericDecodeJson' opts val.signature (genericEncodeJson' opts val.signature val.spine)
    val = runGenericValue genericValue

prop_decoded_spine_valid :: Options -> GenericValue -> Result
prop_decoded_spine_valid opts genericValue =
    annotate check $ "prop_decoded_spin_valid did not hold for: " <> unsafeToString genericValue
  where
    check = Right true == (isValidSpine val.signature <$> genericDecodeJson' opts val.signature (genericEncodeJson' opts val.signature val.spine))
    val = runGenericValue genericValue

checkAesonCompat :: Boolean
checkAesonCompat =
  let
    myTuple = Tuple (Tuple 1 2) "Hello"
    myJust = Just "Test"
    myNothing = Nothing :: Maybe Int
    myLeft = Left "Foo" :: Either String String
    myRight = Right "Bar" :: Either Int String
    unwrapMult = UnwrapTestMult 8 "haha"
    unwrapSingle = UnwrapTestSingle 8
  in
        Aeson.encodeJson myTuple      == fromArray [fromNumber $ toNumber 1, fromNumber $ toNumber 2, fromString "Hello"]
    &&  Aeson.encodeJson myJust       == fromString "Test"
    &&  Aeson.encodeJson myNothing    == jsonNull
    &&  Aeson.encodeJson myLeft       == fromObject (SM.singleton "Left" (fromString "Foo"))
    &&  Aeson.encodeJson myRight      == fromObject (SM.singleton "Right" (fromString "Bar"))
    &&  Aeson.encodeJson unwrapMult   == fromArray [fromNumber $ toNumber 8, fromString "haha"]
    &&  Aeson.encodeJson unwrapSingle == (fromNumber $ toNumber 8)

checkRecordEncoding :: forall eff. Eff (assert :: ASSERT | eff) Unit
checkRecordEncoding = do
    let smallRecord = SmallRecord {foo: "foo", bar: 42}
    let encoded = Aeson.encodeJson smallRecord
    let expected = fromObject $ SM.fromFoldable $
          Tuple "tag" (fromString "SmallRecord") :
          Tuple "foo" (fromString "foo") :
          Tuple "bar" (fromNumber $ toNumber 42) :
          Nil
    assertEquals encoded expected

checkRecordEncodingArgonaut :: forall eff. Eff (assert :: ASSERT | eff) Unit
checkRecordEncodingArgonaut = do
    let smallRecord = SmallRecord {foo: "foo", bar: 42}
    let encoded = Argonaut.encodeJson smallRecord
    let expected = fromObject $ SM.fromFoldable $
          Tuple "tag" (fromString "Test.Main.SmallRecord") :
          Tuple "values"
            (fromArray [fromObject $ SM.fromFoldable $
              Tuple "foo" (fromString "foo") :
              Tuple "bar" (fromNumber $ toNumber 42) :
              Nil
            ]):
          Nil
    assertEquals encoded expected

assertEquals :: forall a eff. (Eq a, Show a) =>
  a -> a -> Eff (assert :: ASSERT | eff) Unit
assertEquals a b = do
  let message = show a <> " /= " <> show b
  assert' message (a == b)

genericsCheck :: forall e. Options -> Eff ( err :: EXCEPTION , random :: RANDOM , console :: CONSOLE, assert :: ASSERT | e) Unit
genericsCheck opts = do
  let vNullary = Nullary2
  let mArgs = MArgs 9 20 "Hello"
  let ntw1 = NewTypeWrapper1 { test : "hello" }
  let ntw2 = NewTypeWrapper2 { test : 9 }
  let mJust = Just "Test"
  let mNothing = Nothing :: Maybe Int
  let mRight = Right 9 :: Either String Int
  let mLeft = Right (Left 2) :: Either String (Either Int Int)
  let mTuple = Tuple (Tuple (Tuple 2 3) "haha") "test"
  let unwrapMult = UnwrapTestMult 8 "haha"
  let unwrapSingle = UnwrapTestSingle 8
  log "Check that decodeJson' and encodeJson' form an isomorphism .."
  assert' " Check all nullary:" (valEncodeDecode opts vNullary)
  assert' " Check multiple args:" (valEncodeDecode opts mArgs)
  assert' " Check new type wrapper (1) encoding:" (valEncodeDecode opts ntw1)
  assert' " Check new type wrapper (2) encoding:" (valEncodeDecode opts ntw2)
  assert' " Check Just" (valEncodeDecode opts mJust)
  assert' " Check Nothing" (valEncodeDecode opts mNothing)
  assert' " Check Right" (valEncodeDecode opts mRight)
  assert' " Check Left" (valEncodeDecode opts mLeft)
  assert' " Check tuple" (valEncodeDecode opts mTuple)
  assert' " Check unwrapMult" (valEncodeDecode opts unwrapMult)
  assert' " Check unwrapSingle" (valEncodeDecode opts unwrapSingle)

  quickCheck $ prop_iso_generic opts
  log "Check that decodeJson' returns a valid spine"
  quickCheck $ prop_decoded_spine_valid opts
  log "Print samples of values encoded with genericEncodeJson"
  print $ genericEncodeJson opts 5
  print $ genericEncodeJson opts [1, 2, 3, 5]
  print $ genericEncodeJson opts (Just "foo")
  print $ genericEncodeJson opts (Right "foo" :: Either String String)
  print $ genericEncodeJson opts $ MyRecord { foo: "foo", bar: 2}
  print $ genericEncodeJson opts "foo"
  print $ genericEncodeJson opts Anonymous
  print $ genericEncodeJson opts $ Guest "guest's handle"
  print $ genericEncodeJson opts $ Registered { name: "user1"
                                   , age: 5
                                   , balance: 26.6
                                   , banned: false
                                   , tweets: ["Hello", "What's up"]
                                   , followers: [ Anonymous
                                                , Guest "someGuest"
                                                , Registered { name: "user2"
                                                             , age: 6
                                                             , balance: 32.1
                                                             , banned: false
                                                             , tweets: ["Hi"]
                                                             , followers: []
                                                             }]}
  print $ genericEncodeJson opts Nullary1
  print $ genericEncodeJson opts Nullary2
  print $ genericEncodeJson opts $ MArgs 9 22 "Test"
  print $ genericEncodeJson opts NArgs
  print $ genericEncodeJson opts ntw1
  print $ genericEncodeJson opts ntw2

  where
    valEncodeDecode :: forall a. (Eq a, Generic a) => Options -> a -> Boolean
    valEncodeDecode opts val = ((Right val) == _) <<< genericDecodeJson opts <<< genericEncodeJson opts $ val


main:: forall e. Eff ( err :: EXCEPTION, random :: RANDOM, console :: CONSOLE, assert :: ASSERT | e ) Unit
main = do
  log "Check Argonaut record encoding"
  checkRecordEncodingArgonaut
  assert' "aesonCompatcheck: " checkAesonCompat
  checkRecordEncoding
  log "genericsCheck check for argonautOptions"
  genericsCheck Argonaut.options
  log "genericsCheck check for aesonOptions"
  genericsCheck Aeson.options
  log "genericsCheck check for encodeSingleOptions"
  let unwrapOpts = case Aeson.options of Options a -> a
  let encodeSingleOptions = Options $ unwrapOpts { encodeSingleConstructors = false }
  genericsCheck encodeSingleOptions

print :: forall a eff. Show a => a -> Eff (console :: CONSOLE | eff) Unit
print = log <<< show

foreign import unsafeToString :: forall obj. obj -> String
