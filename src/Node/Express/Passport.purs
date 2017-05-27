module Node.Express.Passport
  ( module Node.Express.Passport.Common

  , PassportInitializeMiddleware
  , PassportInitializeOptions

  , PassportSessionMiddleware
  , PassportSessionOptions

  , SerializeUser
  , SerializedUser(..)

  , DeserializeUser
  , DeserializedUser(..)

  , getPassport
  , passportInitializeOptions
  , passportInitialize
  , passportInitialize'

  , passportSessionOptions
  , passportSession
  , passportSession'

  , addSerializeUser
  , addDeserializeUser

  , authenticate
  , authenticateOptions
  , AuthenticationMessage(..)
  , OnAuthenticate

  , isAuthenticated
  , logOut
  , logIn
  , loginOptions
  , OnLogin
  , getUser
  )
  where

import Prelude
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut (Json)
import Data.Foreign (Foreign, toForeign)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, mkFn3, mkFn4, runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Node.Express.Handler (HandlerM(..), Handler, runHandlerM)
import Node.Express.Passport.Common (PASSPORT, Passport)
import Node.Express.Types (ExpressM, Request, Response)
import Unsafe.Coerce (unsafeCoerce)


foreign import _getPassport :: forall user eff.
                            Eff (passport :: PASSPORT user | eff) Passport

-- | Initialize and obtain a Passport singleton instance
getPassport :: forall user eff. Eff (passport :: PASSPORT user | eff) Passport
getPassport = _getPassport


type PassportInitializeMiddleware =
  forall eff. Fn3 Request Response (ExpressM eff Unit) (ExpressM eff Unit)
-- | Type of options for `passport.initialize(options);` call
type PassportInitializeOptions = { userProperty :: String }

-- | Default options for `passport.initialize(options);`
-- | By default user object would be stored in the `req.user` property
passportInitializeOptions :: PassportInitializeOptions
passportInitializeOptions = { userProperty: "user" }

foreign import _passportInitialize  :: forall user eff.
                                    Fn2
                                      Passport
                                      PassportInitializeOptions
                                      (Eff (passport :: PASSPORT user | eff) PassportInitializeMiddleware)

-- | Binding for `passport.initialize(options);` call
passportInitialize  :: forall user eff.
                    Passport
                    -> PassportInitializeOptions
                    -> Eff (passport :: PASSPORT user | eff) PassportInitializeMiddleware
passportInitialize = runFn2 _passportInitialize

-- | Binding for `passport.initialize(options);` with default options
passportInitialize' :: forall user eff.
                    Passport
                    -> Eff (passport :: PASSPORT user | eff) PassportInitializeMiddleware
passportInitialize' passport = passportInitialize passport passportInitializeOptions


type PassportSessionMiddleware =
  forall eff. Fn3 Request Response (ExpressM eff Unit) (ExpressM eff Unit)
-- | Type of options for `passport.session(options);` call
type PassportSessionOptions = { pauseStream :: Boolean }

foreign import _passportSession :: forall user eff.
                                Fn2
                                  Passport
                                  PassportSessionOptions
                                  (Eff (passport :: PASSPORT user | eff) PassportSessionMiddleware)

-- | Default options for `passport.session(options);` call
passportSessionOptions :: PassportSessionOptions
passportSessionOptions = { pauseStream: false }

-- | Binding for `passport.session(options);` call
passportSession  :: forall user eff.
                    Passport
                    -> PassportSessionOptions
                    -> Eff (passport :: PASSPORT user | eff) PassportSessionMiddleware
passportSession = runFn2 _passportSession

-- | Binding for `passport.session(options); with default options
passportSession' :: forall user eff.
                    Passport
                    -> Eff (passport :: PASSPORT user | eff) PassportSessionMiddleware
passportSession' passport = passportSession passport passportSessionOptions


type UserSerializedImpl eff =
  Fn2 (Nullable Error) (Nullable Json) (Eff eff Unit)
type SerializeUserImpl user eff =
  Fn3 Request user (UserSerializedImpl eff) (Eff eff Unit)

foreign import _addSerializeUser  :: forall user eff.
                                  Fn2
                                    Passport
                                    (SerializeUserImpl user eff)
                                    (Eff (passport :: PASSPORT user | eff) Unit)

type SerializeUser user eff=
  Request -> user -> Aff eff SerializedUser

data SerializedUser
  = SerializedUser (Maybe Json)
  | SerializePass

addSerializeUser  :: forall user eff.
                  Passport
                  -> SerializeUser user eff
                  -> Eff (passport :: PASSPORT user | eff) Unit
addSerializeUser passport serialize = do
  let
    curryOnSerialized onSerialized error result =
      runFn2 onSerialized (toNullable error) (toNullable result)
    serialize' req user onSerialized =
      void $ runAff onError onSuccess $ serialize req user
      where
      onError :: Error -> Eff eff Unit
      onError error = (curryOnSerialized onSerialized) (Just error) Nothing
      onSuccess :: SerializedUser -> Eff eff Unit
      onSuccess (SerializedUser result) =
        (curryOnSerialized onSerialized) Nothing result
      onSuccess SerializePass =
        (curryOnSerialized onSerialized) (Just $ unsafeCoerce "pass") Nothing
    serialize'' = mkFn3 serialize'
  runFn2 _addSerializeUser passport serialize''


type UserDeserializedImpl user eff =
  Fn2 (Nullable Error) (Nullable user) (Eff eff Unit)
type DeserializeUserImpl user eff =
  Fn3 Request Json (UserDeserializedImpl user eff) (Eff eff Unit)

foreign import _addDeserializeUser  :: forall user eff.
                                    Fn2
                                      Passport
                                      (DeserializeUserImpl user eff)
                                      (Eff (passport :: PASSPORT user | eff) Unit)

type DeserializeUser user eff =
  Request -> Json -> Aff eff (DeserializedUser user)

data DeserializedUser user
  = DeserializedUser (Maybe user)
  | DeserializePass

addDeserializeUser  :: forall user eff.
                    Passport
                    -> DeserializeUser user eff
                    -> Eff (passport :: PASSPORT user | eff) Unit
addDeserializeUser passport deserialize = do
  let
    onDeserialized' onDeserialized error user =
      runFn2 onDeserialized (toNullable error) (toNullable user)
    deserialize' req serialized onDeserialized =
      void $ runAff onError onSuccess $ deserialize req serialized
      where
      onError :: Error -> Eff eff Unit
      onError error =
        (onDeserialized' onDeserialized) (Just error) Nothing
      onSuccess :: DeserializedUser user -> Eff eff Unit
      onSuccess (DeserializedUser user) =
        (onDeserialized' onDeserialized) Nothing user
      onSuccess DeserializePass =
        (onDeserialized' onDeserialized) (Just $ unsafeCoerce "pass") Nothing
    deserialize'' = mkFn3 deserialize'
  runFn2 _addDeserializeUser passport deserialize''


foreign import _authenticate  :: forall user info onAuthEff eff.
                              Fn4
                                Passport
                                String
                                AuthenticateOptionsImpl
                                (Nullable (OnAuthenticateImpl user info onAuthEff))
                                (Fn3 Request Response (ExpressM (passport :: PASSPORT user | eff) Unit) (ExpressM (passport :: PASSPORT user | eff) Unit))

type OnAuthenticateImpl user info eff =
  Fn4 (Nullable Error) (Nullable user) (Nullable info) (Nullable Number) (Eff eff Unit)

type AuthenticateOptionsImpl =
  { session         :: Boolean
  , successRedirect :: Nullable String
  , successMessage  :: Foreign
  , successFlash    :: Foreign
  , failureRedirect :: Nullable String
  , failureMessage  :: Foreign
  , failureFlash    :: Foreign
  , assignProperty  :: Nullable String
  }

type OnAuthenticate user info eff =
  Maybe Error -> Maybe user -> Maybe info -> Maybe Number -> Handler eff

data AuthenticationMessage
  = AuthenticationMessage String
  | StrategyAuthenticationMessage
  | NoAuthenticationMessage

type AuthenticateOptions =
  { session         :: Boolean
  , successRedirect :: Maybe String
  , successMessage  :: AuthenticationMessage
  , successFlash    :: AuthenticationMessage
  , failureRedirect :: Maybe String
  , failureMessage  :: AuthenticationMessage
  , failureFlash    :: AuthenticationMessage
  , assignProperty  :: Maybe String
  }

authenticateOptions :: AuthenticateOptions
authenticateOptions =
  { session:          true
  , successRedirect:  Nothing
  , successMessage:   NoAuthenticationMessage
  , successFlash:     NoAuthenticationMessage
  , failureRedirect:  Nothing
  , failureMessage:   NoAuthenticationMessage
  , failureFlash:     NoAuthenticationMessage
  , assignProperty:   Nothing
  }

authenticate  :: forall info user eff.
              Passport
              -> String
              -> AuthenticateOptions
              -> Maybe (OnAuthenticate user info (passport :: PASSPORT user | eff))
              -> Handler (passport :: PASSPORT user | eff)
authenticate passport strategy options onAuthenticate = HandlerM \req res nxt -> do
  let
    foreignMessage (AuthenticationMessage msg) = toForeign msg
    foreignMessage StrategyAuthenticationMessage = toForeign true
    foreignMessage NoAuthenticationMessage = toForeign $ toNullable Nothing
    optionsImpl =
      { session:          options.session
      , successRedirect:  toNullable options.successRedirect
      , successMessage:   foreignMessage options.successMessage
      , successFlash:     foreignMessage options.successFlash
      , failureRedirect:  toNullable options.failureRedirect
      , failureMessage:   foreignMessage options.failureMessage
      , failureFlash:     foreignMessage options.failureFlash
      , assignProperty:   toNullable options.assignProperty
      }
    convertOnAuthenticate cb err user info status =
      runHandlerM (cb (toMaybe err) (toMaybe user) (toMaybe info) (toMaybe status)) req res nxt
    onAuthenticateImpl = do
      cb <- onAuthenticate
      pure $ mkFn4 $ convertOnAuthenticate cb
    authMiddleware =
      runFn4 _authenticate passport strategy optionsImpl (toNullable onAuthenticateImpl)
  liftEff $ runFn3 authMiddleware req res nxt
  pure unit


foreign import _isAuthenticated :: forall eff. Fn1 Request (Eff eff Boolean)

isAuthenticated :: forall user eff. HandlerM (passport :: PASSPORT user | eff) Boolean
isAuthenticated = HandlerM \req _ _ -> do
  authenticated <- liftEff $ runFn1 _isAuthenticated req
  pure authenticated


foreign import _logIn :: forall user onLoginEff eff.
                      Fn4
                        Request
                        user
                        LoginOptions
                        (Nullable (OnLoginImpl onLoginEff))
                        (Eff (passport :: PASSPORT user | eff) Unit)

type OnLoginImpl eff = Fn1 (Nullable Error) (Eff eff Unit)

type OnLogin eff = Maybe Error -> Handler eff

type LoginOptions = { session :: Boolean }

loginOptions :: LoginOptions
loginOptions = { session: true }

logIn :: forall user eff.
      user
      -> LoginOptions
      -> Maybe (OnLogin (passport :: PASSPORT user | eff))
      -> Handler (passport :: PASSPORT user | eff)
logIn user options onLogin = HandlerM \req res nxt -> do
  let
    onLoginImpl cb = toNullable $ do
      onLoginCb <- cb
      pure $ \err -> runHandlerM (onLoginCb $ toMaybe err) req res nxt
  liftEff $ runFn4 _logIn req user options (onLoginImpl onLogin)
  pure unit


foreign import _logOut :: forall user eff. Fn1 Request (Eff (passport :: PASSPORT user | eff) Unit)

logOut :: forall user eff. HandlerM (passport :: PASSPORT user | eff) Unit
logOut = HandlerM \req _ _ -> do
  liftEff $ runFn1 _logOut req
  pure unit


foreign import _getUser :: forall user eff. Fn1 Request (Eff (passport :: PASSPORT user | eff) (Nullable user))

getUser :: forall user eff. HandlerM (passport :: PASSPORT user | eff) (Maybe user)
getUser = HandlerM \req _ _ -> do
  user <- liftEff $ runFn1 _getUser req
  pure $ toMaybe user
