{-# LANGUAGE BangPatterns
           , FlexibleContexts #-}

module CRF.Alphabet
( Alphabet
, showAlphabet
, readAlphabet
, fromWordsIO
, encodeSent
) where 

import System.Environment (getArgs)
import Control.Exception (throw)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Maybe (catMaybes)
import Data.List (foldl', groupBy, sortBy)
import Data.Function (on)
import qualified Data.Trie as M
import qualified Data.Trie.Convenience as M
import qualified Data.Set as S

import qualified CRF.Base as  B
import qualified CRF.Data.Vect as  V
import CRF.Control.Monad.Lazy (sequence')
import CRF.Control.Eval (seqList, forceList)

type Alphabet =
    ( M.Trie Int        -- observation map
    , [M.Trie Int] )    -- tagset map for each layer

showTrie :: Show a => M.Trie a -> String
showTrie = unlines . map (show . proc) . M.toList
    where proc (k, v) = (v, T.decodeUtf8 k)

readMap :: Read a => String -> M.Trie a
readMap = M.fromListL . map read' . lines where
    read' s =
        let (v, k) = read s
        in v `seq` (T.encodeUtf8 k, v)

showAlphabet :: Alphabet -> String
showAlphabet (obvMap, tagMaps) =
    "#OBSERVATIONS:\n" ++
    showTrie obvMap ++
    (concat $ map (("#LAYER:\n" ++) . showTrie) tagMaps)

readAlphabet :: String -> Alphabet
readAlphabet = makeAlphabet . map readMap . commentSplit where
    commentSplit =
        let splitAt = groupBy (\_ line -> line /= "#OBSERVATIONS:"
                                       && line /= "#LAYER:")
        in map (unlines . tail) . splitAt . lines
    makeAlphabet (obvMap:tagMaps) =
        obvMap `seq` tagMaps `seqList` (obvMap, tagMaps)

encodeWith :: T.Text -> M.Trie Int -> Int
encodeWith x mp = case M.lookup (T.encodeUtf8 x) mp of
    Just k -> k
    Nothing -> B.unknown

encodeObv :: Alphabet -> T.Text -> Int
encodeObv (oa, _) x = x `encodeWith` oa

encodeLabel :: Alphabet -> [T.Text] -> [Int]
encodeLabel (_, ls) xs =
    forceList [x `encodeWith` la | (la, x) <- zip ls xs]

encode :: Alphabet -> B.Word T.Text T.Text -> B.Word Int Int
encode alphabet word = B.newWord obvs' labels'
    where
        obvs' = map encodeObvs $ B.obvs word
        encodeObvs = map (encodeObv alphabet)
        labels' = map encodeLabelProb $ B.labelProbs word
        encodeLabelProb (label, prob) =
            ( V.fromList $ encodeLabel alphabet $ V.toList label
            , prob )

fromWordsIO :: IO [B.Word T.Text T.Text] -> Int -> IO Alphabet
fromWordsIO ioWords ln = do
    let toTrie = M.fromListL . map swap . zip [0..] . map (T.encodeUtf8)
        toTrie' = toTrie . (T.pack ":" :)
        swap (a, b) = (b, a)
    putStr "Computing observations alphabet... "
    obvs <- ioWords >>=
        return . S.toAscList . S.fromList . concat . concat . map B.obvs
    obvs `seqList` putStrLn "done"
    putStr "Observation trie construction... "
    obvMap <- return $ toTrie obvs
    obvMap `seq` putStrLn "done"
    putStr "Computing labels alphabet... "
    tags <- ioWords >>=
        return . S.toAscList . S.fromList . concat . map (V.toList . B.labels)
    tags `seqList` putStrLn "done"
    subtags <- return
        [ S.toAscList $ S.fromList $ map (!! k) $ tags
        | k <- [0 .. ln - 1] ]
    putStr "Label tries construction... "
    tagMaps <- return $ map toTrie' subtags
    tagMaps `seqList` putStrLn "done"
    return (obvMap, tagMaps)

encodeSent :: Alphabet ->  B.Sent T.Text T.Text -> B.Sent Int Int
encodeSent at s =
    B.newSent (B.layersNum s) $ map (encode at) (B.words s)

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
