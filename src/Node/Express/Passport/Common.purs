module Node.Express.Passport.Common
  ( PASSPORT
  , Passport
  )
  where

import Control.Monad.Eff (kind Effect)


-- | Passport effect for specific user type
foreign import data PASSPORT :: Type -> Effect

-- | Passport singleton instance
foreign import data Passport :: Type
