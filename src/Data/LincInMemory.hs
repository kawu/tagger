{-# LANGUAGE MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
	   , DeriveDataTypeable #-}

module Data.LincInMemory
( lincInMemory
) where 

-- import Control.Parallel.Strategies (parBuffer, withStrategy, rseq)
import qualified Data.Text.Lazy.IO as L
import Control.Monad.Lazy (mapM', sequence')
import Control.Applicative ((<$>))
import qualified Data.Array as Arr
import Data.Typeable

import Schema (schematizeSent, LayerCompiled)
import qualified Text.Plain as Plain
import Text.Tagset.Data (Tagset, Label)

import qualified CRF.Base as C
import qualified CRF.Alphabet as A
import CRF.Data.DataSet

data LincInMemory = LincInMemory (Arr.Array Int (C.Sent Int Int))
    deriving (Typeable)

instance DataSet LincInMemory where
    dataSize (LincInMemory arr) = (+1) $ snd $ Arr.bounds $ arr
    readDataElem (LincInMemory arr) k =  return $ arr Arr.! k

alphabetFrom :: Tagset -> [LayerCompiled]
             -> FilePath -> IO A.Alphabet
alphabetFrom tagset layers path = do
    plain <- Plain.parsePlain tagset <$> L.readFile path 
    let linc = map Plain.mkLincSent plain
    let sents = map (C.words . schematizeSent layers) linc
    let alphabet = A.fromWords (concat sents) (length layers)
    return $ alphabet `seq` alphabet

readPath :: Tagset -> [LayerCompiled] -> A.Alphabet
         -> FilePath -> IO [C.Sent Int Int]
readPath tagset layers alphabet path = do
    plain <- Plain.parsePlain tagset <$> L.readFile path 
    let linc = map Plain.mkLincSent plain
    let sents = map (schematizeSent layers) linc
    return (map (A.encodeSent alphabet) sents)

lincInMemory :: Tagset -> [LayerCompiled]-> Maybe A.Alphabet
             -> FilePath -> IO (LincInMemory, A.Alphabet)
lincInMemory tagset layers mAlphabet path = do
    alphabet <- case mAlphabet of
        Just x -> return x
        Nothing -> alphabetFrom tagset layers path
    sents <- readPath tagset layers alphabet path
    let bounds = (0, length sents - 1)
    arr <- return $ Arr.listArray bounds sents
    return $ (LincInMemory arr, alphabet)
