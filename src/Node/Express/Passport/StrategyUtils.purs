module Node.Express.Passport.StrategyUtils where

import Prelude
import Effect (Effect)
import Data.Function.Uncurried (Fn3, runFn3)
import Node.Express.Passport.Types (Passport, PassportStrategy, StrategyId)

foreign import _setStrategy ::
  forall user.
  Fn3
    Passport
    StrategyId
    PassportStrategy
    (Effect Unit)

setStrategy ::
  forall user.
  Passport ->
  StrategyId ->
  PassportStrategy ->
  Effect Unit
setStrategy = runFn3 _setStrategy
