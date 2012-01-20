{-# LANGUAGE MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
	   , DeriveDataTypeable #-}

module Data.LincInMemory
( lincInMemory
) where 

-- import Control.Parallel.Strategies (parBuffer, withStrategy, rseq)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Lazy (mapM', sequence')
import qualified Data.Array as Arr
import Data.Typeable

import Schema (schematizeSent, LayerCompiled)
import qualified Text.Linc.Parser as L
import Text.Tagset.Data (Tagset)

import qualified CRF.Base as C
import qualified CRF.Alphabet as A
import CRF.Data.DataSet

data LincInMemory = LincInMemory (Arr.Array Int (C.Sent Int Int))
    deriving (Typeable)

instance DataSet LincInMemory where
    dataSize (LincInMemory arr) = (+1) $ snd $ Arr.bounds $ arr
    readDataElem (LincInMemory arr) k =  return $ arr Arr.! k

alphabetFrom :: Tagset -> [LayerCompiled] -> [FilePath] -> IO A.Alphabet
alphabetFrom tagset layers paths = do
    sents <- sequence' [ return . C.words . schematizeSent layers 
                         =<< L.parseSent tagset path =<< T.readFile path
                       | path <- paths ]
    alphabet <- return $ A.fromWords (concat sents) (length layers)
    return $ alphabet `seq` alphabet

readPath tagset schema alphabet path = do
    sent <- L.parseSent tagset path
        =<< T.readFile path
    return $ A.encodeSent alphabet
           $ schematizeSent schema sent

lincInMemory :: Tagset -> [LayerCompiled]-> Maybe A.Alphabet
             -> [FilePath] -> IO (LincInMemory, A.Alphabet)
lincInMemory tagset layers mAlphabet paths = do
    alphabet <- case mAlphabet of
        Just x -> return x
        Nothing -> alphabetFrom tagset layers paths
    bounds <- return (0, length paths - 1)
    sents <- mapM' (readPath tagset layers alphabet) paths
    arr <- return $ Arr.listArray bounds sents
    return $ (LincInMemory arr, alphabet)
