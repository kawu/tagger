{-# LANGUAGE MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances #-}

module Data.LincPathDS
( lincPathDS
) where 

-- import Control.Parallel.Strategies (parBuffer, withStrategy, rseq)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Lazy (sequence')

import Schema (schematizeSent, LayerCompiled)
import qualified Text.Linc.Parser as L
import Text.Tagset.Data (Tagset)

import qualified CRF.Base as C
import qualified CRF.Alphabet as A
import CRF.Data.DataSet

data LincPathDS = LincPathDS
    { tagset :: Tagset
    , schema :: [LayerCompiled]
    , alphabet :: A.Alphabet
    , paths :: [FilePath]
    , size :: Int }

instance DataSet LincPathDS where
    dataSize = size
    readDataElem linc k = do
        path <- return $ paths linc !! k
        sent <- L.parseSent (tagset linc) path =<< T.readFile path
        return $ A.encodeSent (alphabet linc)
               $ schematizeSent (schema linc) sent

alphabetFrom :: Tagset -> [LayerCompiled] -> [FilePath] -> IO A.Alphabet
alphabetFrom tagset layers paths = do
    sents <- sequence' [ return . C.words . schematizeSent layers 
                         =<< L.parseSent tagset path =<< T.readFile path
                       | path <- paths ]
            -- >>= return . withStrategy (parBuffer 10 rseq)
    alphabet <- return $ A.fromWords (concat sents) (length layers)
    return $ alphabet `seq` alphabet

lincPathDS :: Tagset -> [LayerCompiled]-> Maybe A.Alphabet
           -> [FilePath] -> IO (LincPathDS, A.Alphabet)
lincPathDS tagset layers mAlphabet paths = do
    alphabet <- case mAlphabet of
        Just x -> return x
        Nothing -> alphabetFrom tagset layers paths
    linc <- return LincPathDS
        { tagset = tagset
        , schema = layers
        , alphabet = alphabet
        , paths = paths
        , size = length paths }
    return (linc, alphabet)
