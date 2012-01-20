{-# LANGUAGE ForeignFunctionInterface #-}

module CRF.LogMath
( logIsZero
, logAdd
, logSum
) where

import Data.List (sort)

foreign import ccall unsafe "math.h log1p"
    log1p :: Double -> Double

inf :: RealFloat a => a
inf = (1/0)

m_inf :: RealFloat a => a
m_inf = -(1/0)

logIsZero :: Double -> Bool
logIsZero x = x == m_inf

logAdd :: Double -> Double -> Double
logAdd x y
    | logIsZero x   = y
    | x > y         = x + log1p(exp(y - x))
    | otherwise     = y + log1p(exp(x - y))

logSub :: Double -> Double -> Double
logSub x y
    | logIsZero x   = y
    | x > y         = x + log1p(exp(y - x))
    | otherwise     = y + log1p(exp(x - y))

--TODO: Which version to use ?
logSum :: [Double] -> Double
--logSum l = foldl (\acc x -> logAdd acc x) m_inf $ sort l
logSum l = foldl (\acc x -> logAdd acc x) m_inf l
