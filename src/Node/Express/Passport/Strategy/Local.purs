module Node.Express.Passport.Strategy.Local where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Aff (Aff, runAff_)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn3, EffectFn4, mkEffectFn4, runEffectFn3)
import Node.Express.Passport.Types (PassportStrategy)
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
  = PassportStrategyLocal__CredentialsVerifiedResult__AuthenticationError
  | PassportStrategyLocal__CredentialsVerifiedResult__Success user

type PassportStrategyLocal__Verify user info
  = Request
  -> Username
  -> Password
  -> Aff
    { result :: PassportStrategyLocal__CredentialsVerifiedResult user
    , info :: Maybe info
    }

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
    runAff_
    (case _ of
          Left error ->
            runEffectFn3
            verified
            (Nullable.notNull error)
            (Nullable.null)
            (Nullable.null)
          Right { result, info } ->
            runEffectFn3
            verified
            Nullable.null
            ( case result of
                  PassportStrategyLocal__CredentialsVerifiedResult__Success user -> Nullable.notNull user
                  _ -> Nullable.null
            )
            (Nullable.toNullable info)
    )
    (verify req username password)
  )

passportStrategyLocal ::
  forall proxy user info.
  proxy user -> proxy info ->
  PassportStrategyLocalOptions ->
  PassportStrategyLocal__Verify user info ->
  PassportStrategy
passportStrategyLocal _ _ = unsafePassportStrategyLocal
