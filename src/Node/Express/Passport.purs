module Node.Express.Passport
  ( module Node.Express.Passport
  , module Node.Express.Passport.Types
  ) where

import Data.Function.Uncurried
import Effect
import Effect.Aff
import Effect.Uncurried
import Foreign
import Node.Express.Passport.Types
import Node.Express.Types
import Prelude

import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Express.Handler (HandlerM(..), Handler, runHandlerM)
import Unsafe.Coerce (unsafeCoerce)

foreign import _getPassport :: Effect Passport

-- | Initialize and obtain a Passport singleton instance
getPassport :: Effect Passport
getPassport = _getPassport

-- | Type of options for `passport.initialize(options);` call
type PassportInitializeOptions
  = { userProperty :: String }

-- | Default options for `passport.initialize(options);`
-- | By default user object would be stored in the `req.user` property
defaultPassportInitializeOptions :: PassportInitializeOptions
defaultPassportInitializeOptions = { userProperty: "user" }

foreign import _passportInitialize :: EffectFn2 Passport PassportInitializeOptions Middleware

-- | Binding for `passport.initialize(options);` call
passportInitialize :: Passport -> PassportInitializeOptions -> Effect Middleware
passportInitialize = runEffectFn2 _passportInitialize

-- | Type of options for `passport.session(options);` call
type PassportSessionOptions
  = { pauseStream :: Boolean }

-- | Default options for `passport.session(options);` call
defaultPassportSessionOptions :: PassportSessionOptions
defaultPassportSessionOptions = { pauseStream: false }

foreign import _passportSession :: EffectFn2 Passport PassportSessionOptions Middleware

-- | Binding for `passport.session(options);` call
passportSession :: forall user.  Passport -> PassportSessionOptions -> Effect Middleware
passportSession = runEffectFn2 _passportSession

type AddSerializeUser__Implementation__SerializerCallback
  = EffectFn2 (Nullable Error) (Nullable Json) Unit

type AddSerializeUser__Implementation__Serializer user
  = EffectFn3 Request user AddSerializeUser__Implementation__SerializerCallback Unit

foreign import _addSerializeUser
  :: forall user
   . EffectFn2
      Passport
      (AddSerializeUser__Implementation__Serializer user)
      Unit

data SerializedUser
  = SerializedUser__Result (Maybe Json)
  | SerializedUser__Pass

-- https://github.com/jaredhanson/passport/blob/2327a36e7c005ccc7134ad157b2f258b57aa0912/lib/authenticator.js#L272
magicPass :: String
magicPass = "pass"

type AddSerializeUser__Callback user = Request -> user -> Aff SerializedUser

addSerializeUser
  :: forall user
   . Passport
  -> AddSerializeUser__Callback user
  -> Effect Unit
addSerializeUser passport serializeAff =
  runEffectFn2
  _addSerializeUser
  passport
  ( mkEffectFn3 \req user callback ->
      runAff_
      ( case _ of
            Left error -> runEffectFn2 callback (Nullable.notNull error) Nullable.null
            Right s ->
              case s of
                    SerializedUser__Result result -> runEffectFn2 callback (Nullable.notNull (unsafeCoerce result)) Nullable.null
                    SerializedUser__Pass -> runEffectFn2 callback (Nullable.notNull (unsafeCoerce magicPass)) Nullable.null
      )
      (serializeAff req user)
  )

type AddDeserializeUser__Implementation__DeserializerCallback user
  = EffectFn2 (Nullable Error) (Nullable user) Unit

type AddDeserializeUser__Implementation__Deserializer user
  = EffectFn3 Request Json (AddDeserializeUser__Implementation__DeserializerCallback user) Unit

foreign import _addDeserializeUser :: forall user. EffectFn2 Passport (AddDeserializeUser__Implementation__Deserializer user) Unit

data DeserializedUser user
  = DeserializedUser__Result (Maybe user)
  | DeserializedUser__Pass

type AddDeserializeUser__Callback user = Request -> Json -> Aff (DeserializedUser user)

addDeserializeUser
  :: forall user
   . Passport
  -> AddDeserializeUser__Callback user
  -> Effect Unit
addDeserializeUser passport deserializeAff =
  runEffectFn2
  _addDeserializeUser
  passport
  ( mkEffectFn3 \req user callback ->
      runAff_
      ( case _ of
            Left error -> runEffectFn2 callback (Nullable.notNull error) Nullable.null
            Right s ->
              case s of
                    DeserializedUser__Result result -> runEffectFn2 callback (Nullable.notNull (unsafeCoerce result)) Nullable.null
                    DeserializedUser__Pass -> runEffectFn2 callback (Nullable.notNull (unsafeCoerce magicPass)) Nullable.null
      )
      (deserializeAff req user)
  )

newtype StrategyId = StrategyId String

type Authenticate__Implementation__Callback user info
  = EffectFn4 (Nullable Error) (Nullable user) (Nullable info) (Nullable Number) Unit

type Authenticate__Implementation__Options
  = { session :: Boolean
    , successRedirect :: Nullable String
    , successMessage :: Foreign
    , successFlash :: Foreign
    , failureRedirect :: Nullable String
    , failureMessage :: Foreign
    , failureFlash :: Foreign
    , assignProperty :: Nullable String
    }

foreign import _authenticate ::
  forall user info .
  Fn4
    Passport
    StrategyId
    Authenticate__Implementation__Options
    (Nullable (Authenticate__Implementation__Callback user info))
    (EffectFn3 Request Response (Effect Unit) Unit)

-- e.g. flash message
data AuthenticationMessage
  = AuthenticationMessage__Custom String
  | AuthenticationMessage__StrategyDefault
  | AuthenticationMessage__Disable

type AuthenticateOptions
  = { session :: Boolean
    , successRedirect :: Maybe String
    , successMessage :: AuthenticationMessage
    , successFlash :: AuthenticationMessage
    , failureRedirect :: Maybe String
    , failureMessage :: AuthenticationMessage
    , failureFlash :: AuthenticationMessage
    , assignProperty :: Maybe String
    }

defaultAuthenticateOptions :: AuthenticateOptions
defaultAuthenticateOptions =
  { session: true
  , successRedirect: Nothing
  , successMessage: AuthenticationMessage__Disable
  , successFlash: AuthenticationMessage__Disable
  , failureRedirect: Nothing
  , failureMessage: AuthenticationMessage__Disable
  , failureFlash: AuthenticationMessage__Disable
  , assignProperty: Nothing
  }

data Authenticate__CustomCallbackResult user
  = Authenticate__CustomCallbackResult__Error Error
  | Authenticate__CustomCallbackResult__AuthenticationError -- user is set to false
  | Authenticate__CustomCallbackResult__Success user

type Authenticate__CustomCallback info user
  = { result :: Authenticate__CustomCallbackResult user
    , info :: Maybe info
    , status :: Maybe Number
    }
    -> Handler

authenticate ::
  forall info user.
  Passport ->
  StrategyId ->
  AuthenticateOptions ->
  Maybe (Authenticate__CustomCallback info user) ->
  Handler
authenticate passport strategyid options onAuthenticate =
  HandlerM \req res nxt -> do
    let
      convertAuthenticationMessage (AuthenticationMessage__Custom msg) = unsafeToForeign msg
      convertAuthenticationMessage AuthenticationMessage__StrategyDefault = unsafeToForeign true
      convertAuthenticationMessage AuthenticationMessage__Disable = unsafeToForeign $ Nullable.null

      optionsImplementation =
        { session:         options.session
        , successRedirect: Nullable.toNullable options.successRedirect
        , successMessage:  convertAuthenticationMessage options.successMessage
        , successFlash:    convertAuthenticationMessage options.successFlash
        , failureRedirect: Nullable.toNullable options.failureRedirect
        , failureMessage:  convertAuthenticationMessage options.failureMessage
        , failureFlash:    convertAuthenticationMessage options.failureFlash
        , assignProperty:  Nullable.toNullable options.assignProperty
        }

    liftEffect $
      runEffectFn3
      ( runFn4
        _authenticate
        passport
        strategyid
        optionsImplementation
        ( case onAuthenticate of
               Just onAuthenticate' -> Nullable.notNull $ mkEffectFn4 \error user info status ->
                  runHandlerM
                  ( onAuthenticate'
                    { result:
                        case Nullable.toMaybe error of
                             Just error' -> Authenticate__CustomCallbackResult__Error error'
                             Nothing ->
                               case Nullable.toMaybe user of
                                    Just user' -> Authenticate__CustomCallbackResult__Success user'
                                    Nothing -> Authenticate__CustomCallbackResult__AuthenticationError
                    , info: Nullable.toMaybe info
                    , status: Nullable.toMaybe status
                    }
                  )
                  req
                  res
                  nxt
               Nothing -> Nullable.null
        )
      )
      req
      res
      nxt

foreign import _isAuthenticated :: EffectFn1 Request Boolean

isAuthenticated :: HandlerM Boolean
isAuthenticated = HandlerM \req _ _ -> liftEffect $ runEffectFn1 _isAuthenticated req

foreign import _logIn ::
  forall user .
  EffectFn4
    Request
    user
    LoginOptions
    (Nullable LogIn__Implementation__CustomCallback)
    Unit

type LogIn__Implementation__CustomCallback = EffectFn1 (Nullable Error) Unit

type LoginOptions
  = { session :: Boolean }

defaultLoginOptions :: LoginOptions
defaultLoginOptions = { session: true }

type LogIn__CustomCallback = Maybe Error -> Handler

logIn ::
  forall user.
  user ->
  LoginOptions ->
  Maybe LogIn__CustomCallback ->
  Handler
logIn user options onLogin =
  HandlerM \req res nxt ->
    liftEffect $
      runEffectFn4
      _logIn
      req
      user
      options
      (case onLogin of
            Just onLogin' -> Nullable.notNull $ mkEffectFn1 \error -> runHandlerM (onLogin' (Nullable.toMaybe error)) req res nxt
            Nothing -> Nullable.null
      )

foreign import _logOut :: EffectFn1 Request Unit

logOut :: HandlerM Unit
logOut = HandlerM \req _ _ -> liftEffect $ runEffectFn1 _logOut req

foreign import _getUser :: forall user . EffectFn1 Request (Nullable user)

getUser :: forall user . HandlerM (Maybe user)
getUser =
  HandlerM \req _ _ -> do
    user <- liftEffect $ runEffectFn1 _getUser req
    pure $ Nullable.toMaybe user
