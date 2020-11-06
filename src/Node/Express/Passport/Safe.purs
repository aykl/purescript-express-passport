module Node.Express.Passport.Safe where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (Error)
import Node.Express.Handler (Handler)
import Node.Express.Passport.Types (Passport, StrategyId)
import Node.Express.Passport.Unsafe (AddDeserializeUser__Callback, AddSerializeUser__Callback, AuthenticateOptions, Authenticate__CustomCallback, LoginOptions, unsafeAddDeserializeUser, unsafeAddSerializeUser, unsafeAuthenticate, unsafeGetUser, unsafeLogIn)
import Node.Express.Types (Request)

getUser :: forall proxy user. proxy user -> Request -> Effect (Maybe user)
getUser _ = unsafeGetUser

logIn ::
  forall proxy user.
  proxy user ->
  user ->
  LoginOptions ->
  Maybe (Maybe Error -> Effect Unit) ->
  Request ->
  Effect Unit
logIn _ = unsafeLogIn

authenticate ::
  forall proxy user info.
  proxy user ->
  proxy info ->
  Passport ->
  StrategyId ->
  AuthenticateOptions ->
  Maybe (Authenticate__CustomCallback info user) ->
  Handler
authenticate _ _ = unsafeAuthenticate

addSerializeUser ::
  forall proxy user.
  proxy user ->
  Passport ->
  AddSerializeUser__Callback user ->
  Effect Unit
addSerializeUser _ = unsafeAddSerializeUser

addDeserializeUser ::
  forall proxy user.
  proxy user ->
  Passport ->
  AddDeserializeUser__Callback user ->
  Effect Unit
addDeserializeUser _ = unsafeAddDeserializeUser
