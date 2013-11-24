{-# LANGUAGE GADTs #-}
module Currency where

data Cash a where
  USD :: (Show a, Num a) => a -> Cash a
  DKK :: (Show a, Num a) => a -> Cash a
  SEK :: (Show a, Num a) => a -> Cash a

instance Show (Cash a) where
  show (USD v) = "USD " ++ show v
  show (DKK v) = "DKK " ++ show v
  show (SEK v) = "SEK " ++ show v

instance (Num a) => Num (Cash a) where
  (USD v0) + (USD v1) = USD (v0+v1)
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

instance (Fractional a) => Fractional (Cash a) where
  (USD v0) / (USD v1) = USD (v0/v1)
  (DKK v0) / (DKK v1) = DKK (v0/v1)
  (SEK v0) / (SEK v1) = SEK (v0/v1)
  _ / _ = error "Conversion error!"
  fromRational _ = undefined

exp :: (Floating a) => Cash a -> a -> Cash a
exp (DKK v) d = DKK $ v**d
exp (SEK v) d = SEK $ v**d
exp (USD v) d = USD $ v**d

scale :: (Num a) => Cash a -> a -> Cash a
scale (DKK v) d = DKK $ v*d
scale (SEK v) d = SEK $ v*d
scale (USD v) d = USD $ v*d

add :: (Num a) => a -> Cash a -> Cash a
add d (DKK v) = DKK $ v+d
add d (SEK v) = SEK $ v+d
add d (USD v) = USD $ v+d
