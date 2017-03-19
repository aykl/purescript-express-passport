module Node.Express.Passport.Common
  ( PASSPORT
  , Passport
  )
  where


-- | Passport effect for specific user type
foreign import data PASSPORT :: * -> !

-- | Passport singleton instance
foreign import data Passport :: *
