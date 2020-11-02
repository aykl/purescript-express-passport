module Node.Express.Passport.Types where

import Data.Newtype (class Newtype)

data Passport

data PassportStrategy

newtype StrategyId = StrategyId String

derive instance newtypeStrategyId :: Newtype StrategyId _
