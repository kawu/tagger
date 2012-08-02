{-# LANGUAGE FlexibleInstances #-}

module CRF.Data.DataSet
( DataSet
, dataSize
, readDataSet
, readDataElem
, readDataElems
) where 

import qualified Data.Vector as V

import CRF.Control.Monad.Lazy (sequence')
import qualified CRF.Base as B

class DataSet s where
    dataSize :: s -> Int
    readDataElem :: s -> Int -> IO (B.Sent Int Int)
    readDataElems :: s -> [Int] -> IO [B.Sent Int Int]
    readDataSet :: s -> IO [B.Sent Int Int]

    -- default implementations
    readDataElems s ks =
        sequence' [readDataElem s k | k <- ks]
            -- >>=  return . withStrategy (parBuffer 10 rseq)
            -- UWAGA: Powyzsza, zakomentowana linia nie ma sensu.
            --        Jestesmy w IO, wiec i tak operacje zostana
            --        wykonane sekwencyjnie !
    readDataSet s = readDataElems s [0 .. dataSize s - 1]

instance DataSet (V.Vector (B.Sent Int Int)) where
    dataSize = V.length
    readDataElem v k = return (v V.! k)
