{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module CRF.Alphabet
( Alphabet
-- , showAlphabet
-- , readAlphabet
, fromWords
, encodeSent
, encodeO
, encodeL
, decodeL
) where 

import System.Environment (getArgs)
import Control.Applicative ((<$>), (<*>))
import Control.Exception (throw)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Maybe (catMaybes)
import Data.List (foldl', groupBy, sortBy)
import Data.Function (on)
import Data.Binary (Binary, get, put)

-- import Control.Parallel (pseq)
-- import Control.Parallel.Strategies (parMap, rseq)

import qualified CRF.Base as  B
import qualified CRF.Data.Vect as  V
import CRF.Control.Monad.Lazy (sequence')
import CRF.Control.Eval (seqList, forceList)

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = return . T.decodeUtf8 =<< get

data Alphabet = Alphabet
    { obMap  :: M.Map T.Text Int          -- observation map
    , lbMaps :: [ ( M.Map T.Text Int      -- label map and reversed map
                  , M.Map Int T.Text ) ] }  --   for each layer
    deriving (Show)

instance Binary Alphabet where
    put codec = do
        put $ obMap codec
        put $ lbMaps codec
    get = Alphabet <$> get <*> get

updateMap :: Ord a => M.Map a Int -> a -> M.Map a Int
updateMap mp x =
  case M.lookup x mp of
    Just k -> mp
    Nothing -> M.insert x n mp
  where
    !n = M.size mp

encodeWith :: (Ord a) => a -> M.Map a Int -> Int
encodeWith x mp = case M.lookup x mp of
    Just k -> k
    Nothing -> B.unknown

updateO :: Alphabet -> T.Text -> Alphabet
updateO codec x =
    let obMap' = updateMap (obMap codec) x
    in  obMap' `seq` codec { obMap = obMap' }

encodeO :: Alphabet -> T.Text -> Int
encodeO codec x = x `encodeWith` obMap codec

updateLE :: (M.Map T.Text Int, M.Map Int T.Text) -> T.Text
         -> (M.Map T.Text Int, M.Map Int T.Text)
updateLE (lbMap, lbMapR) x =
    let lbMap' = updateMap lbMap x
        lbMapR' = M.insert (lbMap' M.! x) x lbMapR
    in  lbMap' `seq` lbMapR' `seq` (lbMap', lbMapR')

updateL :: Alphabet -> [T.Text] -> Alphabet
updateL codec xs =
    let ls = forceList [updateLE la x | (la, x) <- zip (lbMaps codec) xs]
    in  ls `seq` codec { lbMaps = ls }

encodeL :: Alphabet -> [T.Text] -> [Int]
encodeL codec xs = forceList
    [x `encodeWith` la | (la, x) <- zip (map fst $ lbMaps codec) xs]

decodeL :: Alphabet -> [Int] -> [T.Text]
decodeL codec xs = (forceList . catMaybes)
    [x `M.lookup` la | (la, x) <- zip (map snd $ lbMaps codec) xs]

update :: Alphabet -> B.Word T.Text T.Text -> Alphabet
update alphabet0 word =
    alphabet2
  where
    alphabet1 = foldl' updateO alphabet0 $ concat $ B.obvs word
    alphabet2 = foldl' updateL alphabet1 $ V.toList $ B.labels word

encode :: Alphabet -> B.Word T.Text T.Text -> B.Word Int Int
encode alphabet word =
    B.mkWord obvs' labels'
  where
    obvs' = map encodeOs $ B.obvs word
    encodeOs = map (encodeO alphabet)
    labels' = map encodeLabelProb $ B.labelProbs word
    encodeLabelProb (label, prob) =
        ( V.fromList $ encodeL alphabet $ V.toList label
        , prob )
        
fromWords :: [B.Word T.Text T.Text] -> Int -> Alphabet
fromWords ws ln =
    foldl' update init ws
  where
    init = Alphabet M.empty (replicate ln (withDummy, withDummyR))
    withDummy = M.singleton  ":" B.dummy
    withDummyR = M.singleton B.dummy ":"

encodeSent :: Alphabet -> B.Sent T.Text T.Text -> B.Sent Int Int
encodeSent at s = B.mkSent (B.layersNum s)
                $ map (encode at) (B.words s)
-- encodeSent at s = words `pseq` B.newSent (B.layersNum s) words
--     where words = parMap rseq (encode at) (B.words s)

-- TESTING
-- 
-- main = do
--     [tagsetFile, schemaFile, lincFile] <- getArgs
--     tagset <- parseTagset tagsetFile =<< readFile tagsetFile
--     schema <- parseSchema tagset schemaFile =<< readFile schemaFile
--     alphabet <- getAlphabet tagset schema lincFile
--     -- print alphabet
--     encoded <- getEncoded alphabet tagset schema lincFile
--     printList encoded
--     -- printList =<< getSchemed' schema lincFile
-- 
-- getLinc tagset lincFile = parseLinc tagset =<< T.readFile lincFile
-- 
-- getSchemed schema [] = []
-- getSchemed schema (r:xs) =
--     schematizeSent schema r : getSchemed schema xs
-- 
-- getSchemed' tagset schema lincFile = do
--     linc <- getLinc tagset lincFile
--     return $ getSchemed schema linc
-- 
-- getEncoded alphabet tagset schema lincFile = do
--     schemed <- getSchemed' tagset schema lincFile
--     return $ map (encodeSent alphabet) schemed
-- 
-- getAlphabet tagset schema lincFile = do
--     schemed <- getSchemed' tagset schema lincFile
--     words <- return [word | sent <- schemed, word <- sent]
--     return $ fromWords words $ length schema
-- 
-- printList [] = return ()
-- printList (x:xs) = print x >> printList xs
