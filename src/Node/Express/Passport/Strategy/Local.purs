module Node.Express.Passport.Strategy.Local
  ( PassportStrategyLocalOptions
  , passportStrategyLocalOptions

  , CredentialsVerified
  , PassportVerify

  , Username
  , Password
  )
  where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, mkFn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Node.Express.Passport.Strategy.Common (PassportStrategy)
import Node.Express.Types (Request)


type PassportStrategyLocalOptions =
  { usernameField :: String
  , passwordField :: String
  }

passportStrategyLocalOptions :: PassportStrategyLocalOptions
passportStrategyLocalOptions =
  { usernameField: "username", passwordField: "password" }


type Username = String
type Password = String


type CredentialsVerifiedImpl user info eff =
  Fn3 (Nullable Error) (Nullable user) (Nullable info) (Eff eff Unit)
type PassportVerifyImpl user info eff =
  Fn4 Request Username Password (CredentialsVerifiedImpl user info eff) (Eff eff Unit)

type CredentialsVerified user info eff =
  Maybe Error -> Maybe user -> Maybe info -> Eff eff Unit
type PassportVerify user info eff =
  Request -> Username -> Password -> CredentialsVerified user info eff
  -> Eff eff Unit

foreign import _passportStrategyLocal :: forall user info eff. PassportStrategyLocalOptions
                                      -> PassportVerifyImpl user info eff
                                      -> PassportStrategy

passportStrategyLocal :: forall user info eff.
                      PassportStrategyLocalOptions
                      -> PassportVerify user info eff
                      -> PassportStrategy
passportStrategyLocal options verify =
  let
    curryVerified verified error user info =
      runFn3 verified (toNullable error) (toNullable user) (toNullable info)
    verify' req username password verified =
      verify req username password (curryVerified verified)
    verify'' = mkFn4 verify'
  in
  _passportStrategyLocal options verify''

passportStrategyLocal'  :: forall user info eff.
                        PassportVerify user info eff
                        -> PassportStrategy
passportStrategyLocal' = passportStrategyLocal passportStrategyLocalOptions
