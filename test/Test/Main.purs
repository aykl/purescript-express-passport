module Test.Main where

import Effect
import Node.Express.Passport
import Node.Express.Passport.Strategy.Common
import Node.Express.Passport.Strategy.Local
import Prelude

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Console (log)
import Node.Express.Types (Request)

main :: Effect Unit
main = do
  log "You should add some tests."

----------
passportSerializeString :: AddSerializeUser__Callback String
passportSerializeString req user = pure $ SerializedUser__Result $ Just $ encodeJson user

passportDeserializeString :: AddDeserializeUser__Callback String
passportDeserializeString req obj = pure $ either onError onSuccess $ decodeJson obj
  where
  onError = const DeserializedUser__Pass

  onSuccess = DeserializedUser__Result <<< Just

passportSerializeNumber :: AddSerializeUser__Callback Number
passportSerializeNumber req user = pure $ SerializedUser__Result $ Just $ encodeJson user

verify ::
  forall info.
  Request ->
  Username ->
  Password ->
  PassportStrategyLocal__CredentialsVerified String info ->
  Effect Unit
verify req (Username username) password verified =
  verified { result: PassportStrategyLocal__CredentialsVerifiedResult__Success username, info: Nothing }

initPassport :: Effect Passport
initPassport = do
  passport <- getPassport
  setStrategy passport "local" $ passportStrategyLocal defaultPassportStrategyLocalOptions $ verify
  addDeserializeUser passport passportDeserializeString
  addSerializeUser passport passportSerializeString
  -- TODO: This line should cause type error when uncommented
  -- addSerializeUser passport passportSerializeNumber
  pure passport
