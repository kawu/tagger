{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , FunctionalDependencies #-}

module CRF.Data.Vect
( Vect
, len
, (!)
, fromList
, toList
) where

import qualified Data.Array.IArray as A
import qualified Data.Vector as V

class Vect s a | s -> a where
    len :: s -> Int
    (!) :: s -> Int -> a
    fromList :: [a] -> s
    toList :: s -> [a]

instance Vect [a] a where
    len = length
    (!) = (!!)
    fromList = id
    toList = id

instance Vect (A.Array Int a) a where
    len = (+1) . snd . A.bounds
    (!) = (A.!)
    fromList xs = A.listArray (0, length xs - 1) xs
    toList = A.elems

instance Vect (V.Vector a) a where
    len = V.length
    (!) = (V.!)
    fromList = V.fromList
    toList = V.toList
