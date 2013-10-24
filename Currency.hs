{-# LANGUAGE GADTs #-}
module Currency where

data Cash a where
  USD :: (Show a, Num a) => a -> Cash a
  DKK :: (Show a, Num a) => a -> Cash a
  SEK :: (Show a, Num a) => a -> Cash a

instance Show (Cash a) where
  show (USD v) = "USD " ++ show v
  show (DKK v) = "SEK " ++ show v
  show (SEK v) = "DKK " ++ show v

instance Num (Cash a) where
  (USD v0) + (USD v1) = USD (v0 + v1)
  (DKK v0) + (DKK v1) = DKK (v0+v1)
  (SEK v0) + (SEK v1) = SEK (v0+v1)
  _ + _ = error "Conversion error!" 
  (USD v0) * (USD v1) = USD (v0*v1)
  (DKK v0) * (DKK v1) = DKK (v0*v1)
  (SEK v0) * (SEK v1) = SEK (v0*v1)
  _ * _ = error "Conversion error!" 
  abs (USD v) = USD (abs v)
  abs (DKK v) = DKK (abs v)
  abs (SEK v) = SEK (abs v)
  signum _ = undefined
  fromInteger _ = undefined
