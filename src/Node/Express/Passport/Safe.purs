module Node.Express.Passport.Safe where

import Node.Express.Passport.Unsafe as Unsafe
import Node.Express.Passport.Unsafe

import Data.Function.Uncurried
import Effect
import Effect.Aff
import Effect.Uncurried
import Foreign
import Node.Express.Passport.Types
import Node.Express.Types
import Prelude
import Node.Express.Passport.Utils

import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Express.Handler (HandlerM(..), Handler, runHandlerM)
import Unsafe.Coerce (unsafeCoerce)

getUser :: forall proxy user . proxy user -> HandlerM (Maybe user)
getUser _ = unsafeGetUser

logIn
  :: forall proxy user
   . proxy user ->
  user ->
  LoginOptions ->
  Maybe LogIn__CustomCallback ->
  Handler
logIn _ = unsafeLogIn

authenticate
  :: forall proxy user info
  . proxy user ->
    proxy info ->
  Passport ->
  StrategyId ->
  AuthenticateOptions ->
  Maybe (Authenticate__CustomCallback info user) ->
  Handler
authenticate _ _ = unsafeAuthenticate

addSerializeUser
  :: forall proxy user
   . proxy user
  -> Passport
  -> AddSerializeUser__Callback user
  -> Effect Unit
addSerializeUser _ = unsafeAddSerializeUser

addDeserializeUser
  :: forall proxy user
   . proxy user
  -> Passport
  -> AddDeserializeUser__Callback user
  -> Effect Unit
addDeserializeUser _ = unsafeAddDeserializeUser
