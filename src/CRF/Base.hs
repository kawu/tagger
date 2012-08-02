module CRF.Base
( Word
, mkWord
, Sent
, mkSent
, sentLen
, words
, word
, layersNum
, obvs
, labelProbs
, labels
, probs
, labelIxs
, labelIxs2
, labelIxs3
, labelProbsOn
, labelsOn
, interp
, interpsNum
, dummy
, unknown
) where

import Prelude hiding (words)
import qualified Data.Array.IArray as A

import qualified CRF.Data.Vect as V
import CRF.Control.Eval (seqList, seqList2)

-- Word:
--   list of observations per layers,
--   labels with corresponding probabilities.
data Word o t = Word [[o]]
                     (A.Array Int [t])
                     (A.Array Int Double)
    deriving (Show)

mkWord :: [[o]] -> [([t], Double)] -> Word o t
mkWord os ts =
    os `seqList2` labels' `seq` probs' `seq` Word os labels' probs'
    where (labels, probs) = unzip ts
    	  labels' = labels `seqList2` V.fromList labels
          probs'  = probs `seqList` V.fromList probs

obvs :: Word o t -> [[o]]
obvs (Word os _ _) = os

labels :: Word o t -> A.Array Int [t]
labels (Word _ ts _) = ts

probs :: Word o t -> A.Array Int Double
probs (Word _ _ ps) = ps

labelProbs :: Word o t -> [([t], Double)]
labelProbs word = zip (V.toList $ labels word) (V.toList $ probs word)

-- labelProb :: Word o t -> Int -> ([t], Double)
-- labelProb w = (labelProbs w V.!)
-- 
-- label w = fst . labelProb w
-- prob  w = snd . labelProb w

data Sent o t = Sent Int (A.Array Int (Word o t))
    deriving Show

mkSent :: Int -> [Word o t] -> Sent o t
mkSent ln ws =
    -- ln `seq` ws `seqList`
    Sent ln $ V.fromList ws

sentLen :: Sent o t -> Int
sentLen (Sent _ ws) = V.len ws

layersNum :: Sent o t -> Int
layersNum (Sent ln _) = ln

words :: Sent o t -> [Word o t]
words (Sent _ ws) = V.toList $ ws

word :: Sent o t -> Int -> Word o t
word (Sent _ ws) = (ws V.!)

-- unknown: wartosc (obserwacja, pod-tag) nieznana
unknown :: Int
unknown = -1

-- UWAGA: nie nalezy zmieniac wartosci stalej dummy;
-- w razie ewentualnej zmiany nalezy zmodyfikowac modul
-- Data.Alphabet (zob. funkcje encodeWith i fromWords).
--
-- dummy: numer etykiety przed pierwszym slowem zdania
dummy :: Int
dummy = 0

labelsOn :: Sent o Int -> Int -> A.Array Int [Int]
labelsOn s k
    | k < 0 = V.fromList [[dummy | _ <- [1..layersNum s]]]
    | otherwise = labels $ word s k

labelProbsOn :: Sent o Int -> Int -> [([Int], Double)]
labelProbsOn s k
    | k < 0 = ([dummy | _ <- [1..layersNum s]], 1.0) : []
    | otherwise = labelProbs $ word s k

labelIxs :: Sent o t -> Int -> [Int]
labelIxs s k
    | k < 0 = [0]
    | Word _ ts _ <- word s k = [0 .. V.len ts - 1] 
labelIxs2 s k = 
    [(i, j) | i <- labelIxs s k
            , j <- labelIxs s $ k - 1]
labelIxs3 s k =
    [(i, j, h) | i <- labelIxs s k
               , j <- labelIxs s $ k - 1
               , h <- labelIxs s $ k - 2]

interpsNum :: Sent o Int -> Int -> Int
interpsNum sent = V.len . labelsOn sent

interp :: Sent o Int -> Int -> Int -> [Int]
interp sent i = (labelsOn sent i V.!)
