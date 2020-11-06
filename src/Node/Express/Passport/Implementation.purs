module Node.Express.Passport.Implementation where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.Express.Handler (HandlerM(..))
import Node.Express.Passport.Types (Passport)
import Node.Express.Types (Middleware, Request)

-- | Initialize and obtain a Passport singleton instance
foreign import getPassport :: Effect Passport

------------------------------------------------------------------------------------------------------------------------
-- | Type of options for `passport.initialize(options);` call
type PassportInitializeOptions
  = { userProperty :: String }

-- | Default options for `passport.initialize(options);`
-- | By default user object would be stored in the `req.user` property
defaultPassportInitializeOptions :: PassportInitializeOptions
defaultPassportInitializeOptions = { userProperty: "user" }

foreign import _passportInitialize :: Fn2 Passport PassportInitializeOptions Middleware

-- | Binding for `passport.initialize(options);` call
passportInitialize :: Passport -> PassportInitializeOptions -> Middleware
passportInitialize = runFn2 _passportInitialize

------------------------------------------------------------------------------------------------------------------------
-- | Type of options for `passport.session(options);` call
type PassportSessionOptions
  = { pauseStream :: Boolean }

-- | Default options for `passport.session(options);` call
defaultPassportSessionOptions :: PassportSessionOptions
defaultPassportSessionOptions = { pauseStream: false }

foreign import _passportSession :: Fn2 Passport PassportSessionOptions Middleware

-- | Binding for `passport.session(options);` call
passportSession :: Passport -> PassportSessionOptions -> Middleware
passportSession = runFn2 _passportSession

------------------------------------------------------------------------------------------------------------------------
foreign import _isAuthenticated :: EffectFn1 Request Boolean

isAuthenticated :: HandlerM Boolean
isAuthenticated = HandlerM \req _ _ -> liftEffect $ runEffectFn1 _isAuthenticated req

------------------------------------------------------------------------------------------------------------------------
foreign import _logOut :: EffectFn1 Request Unit

logOut :: HandlerM Unit
logOut = HandlerM \req _ _ -> liftEffect $ runEffectFn1 _logOut req
