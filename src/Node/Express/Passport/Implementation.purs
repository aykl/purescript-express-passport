module Node.Express.Passport.Implementation where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Node.Express.Passport.Types (Passport)
import Node.Express.Types (Middleware, Request)
import Prelude

import Effect.Class (liftEffect)
import Node.Express.Handler (HandlerM(..))

foreign import _getPassport :: Effect Passport

-- | Initialize and obtain a Passport singleton instance
getPassport :: Effect Passport
getPassport = _getPassport

------------------------------------------------------------------------------------------------------------------------

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

------------------------------------------------------------------------------------------------------------------------

-- | Type of options for `passport.session(options);` call
type PassportSessionOptions
  = { pauseStream :: Boolean }

-- | Default options for `passport.session(options);` call
defaultPassportSessionOptions :: PassportSessionOptions
defaultPassportSessionOptions = { pauseStream: false }

foreign import _passportSession :: EffectFn2 Passport PassportSessionOptions Middleware

-- | Binding for `passport.session(options);` call
passportSession :: Passport -> PassportSessionOptions -> Effect Middleware
passportSession = runEffectFn2 _passportSession

------------------------------------------------------------------------------------------------------------------------

foreign import _isAuthenticated :: EffectFn1 Request Boolean

isAuthenticated :: HandlerM Boolean
isAuthenticated = HandlerM \req _ _ -> liftEffect $ runEffectFn1 _isAuthenticated req

------------------------------------------------------------------------------------------------------------------------

foreign import _logOut :: EffectFn1 Request Unit

logOut :: HandlerM Unit
logOut = HandlerM \req _ _ -> liftEffect $ runEffectFn1 _logOut req
