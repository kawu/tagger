{-# LANGUAGE MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances #-}

module Data.LincDataSet
( lincPathsDataSet
, lincPathsDataSet'
, getAlphabet
) where 

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Monad.Lazy (sequence')

import Schema (schematizeSent, LayerCompiled)
import qualified Text.Linc.Parser as L
import Text.Tagset.Data (Tagset)

import qualified CRF.Base as C
import qualified CRF.Alphabet as C
import CRF.Data.DataSet

data LincPathsDataSet =
    LincPathsDataSet Tagset [LayerCompiled] C.Alphabet [FilePath]

instance DataSet LincPathsDataSet where
    dataSize (LincPathsDataSet _ _ _ paths) = length paths
    readDataElem (LincPathsDataSet tagset layers alphabet paths) k = do
        path <- return $ paths !! k
        sent <- L.parseSent tagset path =<< T.readFile path
        return $ C.encodeSent alphabet $ schematizeSent layers sent

alphabetFrom :: Tagset -> [LayerCompiled] -> [FilePath] -> IO C.Alphabet
alphabetFrom tagset layers paths = do
    words <- return $ readWords tagset layers paths
    alphabet <- C.fromWordsIO words (length layers)
    return alphabet

readWords :: Tagset -> [LayerCompiled] -> [FilePath] -> IO [C.Word T.Text T.Text]
readWords tagset layers paths = 
    sequence' [ return . C.words . schematizeSent layers 
                =<< L.parseSent tagset path =<< T.readFile path
              | path <- paths ] >>= return . concat

lincPathsDataSet :: Tagset -> [LayerCompiled] -> [FilePath] -> IO LincPathsDataSet
lincPathsDataSet tagset layers paths = do
    alphabet <- alphabetFrom tagset layers paths
    return $ LincPathsDataSet tagset layers alphabet paths

lincPathsDataSet' :: Tagset -> [LayerCompiled] -> [FilePath] -> C.Alphabet
                  -> IO LincPathsDataSet
lincPathsDataSet' tagset layers paths alphabet = do
    return $ LincPathsDataSet tagset layers alphabet paths

getAlphabet :: LincPathsDataSet -> C.Alphabet
getAlphabet (LincPathsDataSet _ _ alphabet _) = alphabet
