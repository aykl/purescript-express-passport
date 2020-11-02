module Test.Main where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Node.Express.Passport (AddDeserializeUser__Callback, AddSerializeUser__Callback, DeserializedUser(..), Passport, SerializedUser(..), getPassport, setStrategy, PassportStrategy)
import Node.Express.Passport as Passport
import Node.Express.Passport.Strategy.Local (PassportStrategyLocal__CredentialsVerifiedResult(..), Password, Username(..), defaultPassportStrategyLocalOptions)
import Node.Express.Passport.Strategy.Local as Passport.Local
import Node.Express.Types (Request)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  log "You should add some tests."

------------------------------------------------------------
-- fixed passport methods for type-safety
passportMethods ::
  { addDeserializeUser :: Passport -> (Request -> Json -> Aff (DeserializedUser String)) -> Effect Unit
  , addSerializeUser :: Passport -> (Request -> String -> Aff SerializedUser) -> Effect Unit
  , passportStrategyLocal ::
    { passwordField :: String
    , usernameField :: String
    } ->
    ( Request ->
      Username ->
      Password ->
      Aff
        { info :: Maybe Void
        , result :: PassportStrategyLocal__CredentialsVerifiedResult String
        }
    ) ->
    PassportStrategy
  }
passportMethods =
  let
    proxyUser :: Proxy String
    proxyUser = Proxy

    proxyInfo :: Proxy Void
    proxyInfo = Proxy
  in
    { addSerializeUser: Passport.addSerializeUser proxyUser
    , addDeserializeUser: Passport.addDeserializeUser proxyUser
    , passportStrategyLocal: Passport.Local.passportStrategyLocal proxyUser proxyInfo
    }

------------------------------------------------------------
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
  Aff
    { result :: PassportStrategyLocal__CredentialsVerifiedResult String
    , info :: Maybe info
    }
verify req (Username username) password = pure { result: PassportStrategyLocal__CredentialsVerifiedResult__Success username, info: Nothing }

initPassport :: Effect Passport
initPassport = do
  passport <- getPassport
  setStrategy passport "local" $ passportMethods.passportStrategyLocal defaultPassportStrategyLocalOptions $ verify
  passportMethods.addDeserializeUser passport passportDeserializeString
  passportMethods.addSerializeUser passport passportSerializeString
  -- TODO: This line should cause type error when uncommented
  -- addSerializeUser passport passportSerializeNumber
  pure passport
