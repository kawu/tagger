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
import Control.Applicative ((<$>))
import qualified Data.Array as Arr
import Data.Typeable

import Schema (schematizeSent, LayerCompiled)
import qualified Text.Plain as Plain
import Text.Tagset.Data (Tagset, Label)

import qualified CRF.Base as C
import qualified CRF.Alphabet as A
import CRF.Data.DataSet

type Unknown = [Label]

data LincInMemory = LincInMemory (Arr.Array Int (C.Sent Int Int))
    deriving (Typeable)

instance DataSet LincInMemory where
    dataSize (LincInMemory arr) = (+1) $ snd $ Arr.bounds $ arr
    readDataElem (LincInMemory arr) k =  return $ arr Arr.! k

alphabetFrom :: Tagset -> Unknown -> [LayerCompiled]
             -> FilePath -> IO A.Alphabet
alphabetFrom tagset unk layers path = do
    plain <- Plain.parsePlain tagset unk <$> readFile path 
    let linc = map Plain.mkLincSent plain
    let sents = map (C.words . schematizeSent layers) linc
    let alphabet = A.fromWords (concat sents) (length layers)
    return $ alphabet `seq` alphabet

readPath :: Tagset -> Unknown -> [LayerCompiled] -> A.Alphabet
         -> FilePath -> IO [C.Sent Int Int]
readPath tagset unk layers alphabet path = do
    plain <- Plain.parsePlain tagset unk <$> readFile path 
    let linc = map Plain.mkLincSent plain
    let sents = map (schematizeSent layers) linc
    return (map (A.encodeSent alphabet) sents)

lincInMemory :: Tagset -> Unknown -> [LayerCompiled]-> Maybe A.Alphabet
             -> FilePath -> IO (LincInMemory, A.Alphabet)
lincInMemory tagset unk layers mAlphabet path = do
    alphabet <- case mAlphabet of
        Just x -> return x
        Nothing -> alphabetFrom tagset unk layers path
    sents <- readPath tagset unk layers alphabet path
    let bounds = (0, length sents - 1)
    arr <- return $ Arr.listArray bounds sents
    return $ (LincInMemory arr, alphabet)
