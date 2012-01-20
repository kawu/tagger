{-# LANGUAGE BangPatterns #-}

module CRF.Alphabet
( Alphabet
-- , showAlphabet
-- , readAlphabet
, fromWords
, encodeSent
) where 

import System.Environment (getArgs)
import Control.Exception (throw)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
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

type Alphabet =
    ( Map.Map T.Text Int        -- observation map
    , [Map.Map T.Text Int] )    -- tagset map for each layer

-- showMap :: (Show k, Show a, Ord a) => Map.Map k a -> String
-- showMap = unlines . map (show . swap) . sortBy (compare `on` snd) . Map.assocs
--     where swap (a, b) = (b, a)
-- 
-- readMap :: (Read k, Read a, Ord k) => String -> Map.Map k a
-- readMap = Map.fromList . map read' . lines where
--     read' s =
--         let (val, key) = read s
--         in key `seq` val `seq` (key, val)
-- 
-- showAlphabet :: Alphabet -> String
-- showAlphabet (obvMap, tagMaps) =
--     "#OBSERVATIONS:\n" ++
--     showMap obvMap ++
--     (concat $ map (("#LAYER:\n" ++) . showMap) tagMaps)
-- 
-- readAlphabet :: String -> Alphabet
-- readAlphabet = makeAlphabet . map readMap . commentSplit where
--     commentSplit =
--         let splitAt = groupBy (\_ line -> line /= "#OBSERVATIONS:"
--                                        && line /= "#LAYER:")
--         in map (unlines . tail) . splitAt . lines
--     makeAlphabet (obvMap:tagMaps) =
--         obvMap `seq` tagMaps `seqList` (obvMap, tagMaps)

updateMap :: (Ord a) => Map.Map a Int -> a -> Map.Map a Int
updateMap mp x = case Map.lookup x mp of
    Just k -> mp
    Nothing -> Map.insert x n mp
    where !n = Map.size mp

encodeWith :: (Ord a) => a -> Map.Map a Int -> Int
encodeWith x mp = case Map.lookup x mp of
    Just k -> k
    Nothing -> B.unknown

updateObv :: Alphabet -> T.Text -> Alphabet
updateObv (oa, la) x = x `seq` oa' `seq` (oa', la)
    where oa' = updateMap oa x

encodeObv :: Alphabet -> T.Text -> Int
encodeObv (oa, _) x = x `encodeWith` oa

updateLabel :: Alphabet -> [T.Text] -> Alphabet
updateLabel (oa, ls) xs = (oa, ls')
    where !ls' = forceList [updateMap la x | (la, x) <- zip ls xs]

encodeLabel :: Alphabet -> [T.Text] -> [Int]
encodeLabel (_, ls) xs =
    forceList [x `encodeWith` la | (la, x) <- zip ls xs]

update :: Alphabet -> B.Word T.Text T.Text -> Alphabet
update alphabet0 word = alphabet2
    where
        alphabet1 = foldl' updateObv alphabet0 $ concat $ B.obvs word
        alphabet2 = foldl' updateLabel alphabet1 $ V.toList $ B.labels word

encode :: Alphabet -> B.Word T.Text T.Text -> B.Word Int Int
encode alphabet word = B.newWord obvs' labels'
    where
        obvs' = map encodeObvs $ B.obvs word
        encodeObvs = map (encodeObv alphabet)
        labels' = map encodeLabelProb $ B.labelProbs word
        encodeLabelProb (label, prob) =
            ( V.fromList $ encodeLabel alphabet $ V.toList label
            , prob )
        
fromWords :: [B.Word T.Text T.Text] -> Int -> Alphabet
fromWords ws ln = foldl' update (Map.empty, replicate ln withDummy) ws
    where withDummy = Map.singleton (T.pack ":") B.dummy

encodeSent :: Alphabet -> B.Sent T.Text T.Text -> B.Sent Int Int
encodeSent at s = B.newSent (B.layersNum s)
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
