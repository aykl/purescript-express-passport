module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Node.Express.Passport (DeserializeUser, DeserializedUser(..), PASSPORT, Passport, SerializeUser, SerializedUser(..), addDeserializeUser, addSerializeUser, getPassport)
import Node.Express.Passport.Strategy (setStrategy)
import Node.Express.Passport.Strategy.Local (CredentialsVerified, passportStrategyLocal')
import Node.Express.Types (Request)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."


type UserString = String

passportSerializeString :: forall eff. SerializeUser UserString eff
passportSerializeString req user =
  pure $ SerializedUser $ Just $ encodeJson user


passportDeserializeString :: forall eff. DeserializeUser UserString eff
passportDeserializeString req obj = pure $ either onError onSuccess $ decodeJson obj
  where
  onError = const DeserializePass
  onSuccess = DeserializedUser <<< Just

type UserNumber = Number

passportSerializeNumber :: forall eff. SerializeUser UserNumber eff
passportSerializeNumber req user =
  pure $ SerializedUser $ Just $ encodeJson user

verify  :: forall info eff.
        Request
        -> String -> String
        -> CredentialsVerified UserString info eff
        -> Eff eff Unit
verify req username password verified = do
  void $ verified Nothing (Just username) Nothing


initPassport  :: forall eff.
              Eff ( passport  :: PASSPORT UserString | eff ) Passport
initPassport = do
  passport <- getPassport
  setStrategy passport "local" $ passportStrategyLocal' $ verify
  addDeserializeUser passport passportDeserializeString
  addSerializeUser passport passportSerializeString
  -- This line should cause type error when uncommented
  -- addSerializeUser passport passportSerializeNumber
  pure passport
