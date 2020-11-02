module Node.Express.Passport.Strategy.Local where

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, EffectFn4, mkEffectFn4, runEffectFn3)
import Node.Express.Passport.Types (PassportStrategy)
import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Exception (Error)
import Node.Express.Types (Request)

type PassportStrategyLocalOptions
  = { usernameField :: String
    , passwordField :: String
    }

defaultPassportStrategyLocalOptions :: PassportStrategyLocalOptions
defaultPassportStrategyLocalOptions = { usernameField: "username", passwordField: "password" }

newtype Username
  = Username String

derive instance newtypeUsername :: Newtype Username _

newtype Password
  = Password String

derive instance newtypePassword :: Newtype Password _

------------------------------------------------------------------------------------------------------------------------

type PassportStrategyLocal__Implementation__CredentialsVerified user info
  = EffectFn3 (Nullable Error) (Nullable user) (Nullable info) Unit

type PassportStrategyLocal__Implementation__Verify user info
  = EffectFn4 Request Username Password (PassportStrategyLocal__Implementation__CredentialsVerified user info) Unit

data PassportStrategyLocal__CredentialsVerifiedResult user
  = PassportStrategyLocal__CredentialsVerifiedResult__Error Error
  | PassportStrategyLocal__CredentialsVerifiedResult__AuthenticationError
  | PassportStrategyLocal__CredentialsVerifiedResult__Success user

type PassportStrategyLocal__CredentialsVerified user info
  = { result :: PassportStrategyLocal__CredentialsVerifiedResult user
    , info :: Maybe info
    }
    -> Effect Unit

type PassportStrategyLocal__Verify user info
  = Request -> Username -> Password -> PassportStrategyLocal__CredentialsVerified user info -> Effect Unit

foreign import _passportStrategyLocal ::
  forall user info.
  Fn2
  PassportStrategyLocalOptions
  (PassportStrategyLocal__Implementation__Verify user info)
  PassportStrategy

unsafePassportStrategyLocal ::
  forall user info.
  PassportStrategyLocalOptions ->
  PassportStrategyLocal__Verify user info ->
  PassportStrategy
unsafePassportStrategyLocal options verify =
  runFn2
  _passportStrategyLocal
  options
  (mkEffectFn4 \req username password verified ->
    verify
    req
    username
    password
    (\{ result, info } ->
      runEffectFn3
      verified
      ( case result of
             PassportStrategyLocal__CredentialsVerifiedResult__Error error -> Nullable.notNull error
             _ -> Nullable.null
      )
      ( case result of
             PassportStrategyLocal__CredentialsVerifiedResult__Success user -> Nullable.notNull user
             _ -> Nullable.null
      )
      (Nullable.toNullable info)
    )
  )

passportStrategyLocal ::
  forall proxy user info.
  proxy user -> proxy info ->
  PassportStrategyLocalOptions ->
  PassportStrategyLocal__Verify user info ->
  PassportStrategy
passportStrategyLocal _ _ = unsafePassportStrategyLocal
