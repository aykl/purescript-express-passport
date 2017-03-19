module Node.Express.Passport.Strategy.Common
  ( PassportStrategy
  , setStrategy
  )
  where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn3, runFn3)
import Node.Express.Passport.Common (PASSPORT, Passport)


foreign import data PassportStrategy :: *

foreign import _setStrategy :: forall user eff.
                            Fn3
                              Passport
                              String
                              PassportStrategy
                              (Eff (passport :: PASSPORT user | eff) Unit)


setStrategy :: forall user eff.
            Passport
            -> String
            -> PassportStrategy
            -> Eff (passport :: PASSPORT user | eff) Unit
setStrategy = runFn3 _setStrategy
