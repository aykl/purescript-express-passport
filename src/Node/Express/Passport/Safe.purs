module Node.Express.Passport.Safe where

import Node.Express.Passport.Unsafe (AddDeserializeUser__Callback, AddSerializeUser__Callback, AuthenticateOptions, Authenticate__CustomCallback, LogIn__CustomCallback, LoginOptions, unsafeAddDeserializeUser, unsafeAddSerializeUser, unsafeAuthenticate, unsafeGetUser, unsafeLogIn)

import Effect (Effect)
import Node.Express.Passport.Types (Passport, StrategyId)
import Prelude

import Data.Maybe (Maybe)
import Node.Express.Handler (Handler, HandlerM)import Node.Express.Handler (Handler, HandlerM)

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
