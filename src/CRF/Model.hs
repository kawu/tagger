{-# LANGUAGE FlexibleContexts #-}

module CRF.Model
( Model
, fromList
, toList
, makeModel
-- , showModel
-- , readModel
, modelSize
-- , prob
-- , cll
, tag
, tag'
, tagProbs
, tagProbs'
, accuracy
, accuracy'
, expectedFeaturesIn
--, gradientOn
) where

import Debug.Trace (trace)
import Data.MemoCombinators (memo3, integral)

import Prelude hiding (words)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.List (zip4, groupBy, sortBy, maximumBy, transpose)
import Data.Function (on)

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies ( using, parList, parBuffer, evalList
                                   , evalTuple2, rseq, parMap )

import CRF.LogMath (logSum)
import CRF.Model.Internal
import CRF.Base
import CRF.Feature
import qualified CRF.Control.DynamicProgramming as DP
import qualified CRF.Data.Vect as V
import CRF.Util (partition)

type ProbArray = Int -> Int -> Int -> Double
type AccF = [Double] -> Double
type WordObservations = [[Int]]
type Label = [Int]

-- interface on top of internal implementation

onWord :: V.Vect t Int => Model -> WordObservations -> t -> Double
onWord crf obs u = 
    sum [ onOFeat k o x crf
        | (k, os, x) <- zip3 [0..] obs (V.toList u)
        , o <- os ]

onTransition :: V.Vect t Int => Model -> t -> t -> t -> Double
onTransition crf u v w =
    sum [ onTFeat k x y z crf
        | (k, x, y, z) <- zip4 [0..] (V.toList u) (V.toList v) (V.toList w) ]

phi :: V.Vect t Int => Model -> WordObservations -> t -> t -> t -> Double
phi crf w a b c = onWord crf w a + onTransition crf a b c

-- phiOn :: V.Vect t Int => Model -> Sent Int t -> Int -> Double
-- phiOn crf sent k =
--     phi crf w a b c
--     where (w, a, _) = sent !! k
--           b = label sent $ k - 1
--           c = label sent $ k - 2

-- more general methods

computePsiMem crf sent i = (Array.!) $
    Array.array bounds [ (k, psi crf x k)
                       | k <- labelIxs sent i ]
    where psi crf x = onWord crf (obvs x) . interp sent i
          bounds = (0, interpsNum sent i - 1)
          x = word sent i

forward :: AccF -> Model -> Sent Int Int -> ProbArray
forward acc crf sent = alpha where
    alpha = DP.flexible3 (-1, sentLen sent - 1)
                (\i   -> (0, interpsNum sent i - 1))
                (\i _ -> (0, interpsNum sent (i - 1) - 1))
                (\t i -> withMem (computePsiMem crf sent i) t i)
    withMem psiMem alpha i j k
        | i == -1 = 0.0
        | otherwise = acc
            [ alpha (i - 1) k h + psiMem j
            + onTransition crf a b (c h)
            | h <- labelIxs sent (i - 2) ]
        where a = interp sent i       j
              b = interp sent (i - 1) k
              c = interp sent (i - 2)

backward :: AccF -> Model -> Sent Int Int -> ProbArray
backward acc crf sent = beta where
    beta = DP.flexible3 (0, sentLen sent)
               (\i   -> (0, interpsNum sent (i - 1) - 1))
               (\i _ -> (0, interpsNum sent (i - 2) - 1))
               (\t i -> withMem (computePsiMem crf sent i) t i)
    withMem psiMem beta i j k
        | i == sentLen sent = 0.0
        | otherwise = acc
            [ beta (i + 1) h j + psiMem h
            + onTransition crf (c h) a b
            | h <- labelIxs sent i ]
        where a = interp sent (i - 1) j
              b = interp sent (i - 2) k
              c = interp sent i

zxBeta :: ProbArray -> Double
zxBeta beta = beta 0 0 0

zxAlpha :: ([Double] -> Double) -> Sent Int Int -> ProbArray -> Double
zxAlpha acc sent alpha =
    acc [ alpha (n - 1) i j
           | (i, j) <- labelIxs2 sent (n - 1) ]
    where n = sentLen sent

zx :: Model -> Sent Int Int -> Double
zx crf = zxBeta . backward logSum crf

zx' :: Model -> Sent Int Int -> Double
zx' crf sent = zxAlpha logSum sent $ forward logSum crf sent

-- --------------------------------------------------------------
argmax :: (Ord b) => (a -> b) -> [a] -> (a, b)
argmax f l = foldl1 choice $ map (\x -> (x, f x)) l
    where choice (x1, v1) (x2, v2)
              | v1 > v2 = (x1, v1)
              | otherwise = (x2, v2)

memoTag :: Model -> Sent Int Int -> [Int]
memoTag crf sent = snd $ alpha 0 0 0 where
    n = sentLen sent
    alpha = memo3 integral integral integral alpha' 
    alpha' i v w
        | i == n = (0, [])
        | otherwise = maximum [ (phi' i u v w, u) `plus` alpha (i + 1) u v
                              | u <- labelIxs sent i ]
    plus (v, x) (v', xs) = (v + v', x : xs)
    phi' i u v w = phi crf (obvs x) a b c
        where x = word sent i
              a = interp sent i       u
              b = interp sent (i - 1) v
              c = interp sent (i - 2) w

dynamicTag :: Model -> Sent Int Int -> [Int]
dynamicTag crf sent = collectMaxArg (0, 0, 0) [] mem where
    mem = DP.flexible3 (0, sentLen sent)
                       (\i   -> (0, interpsNum sent (i - 1) - 1))
                       (\i _ -> (0, interpsNum sent (i - 2) - 1))
                       (\t i -> withMem (computePsiMem crf sent i) t i)
    withMem psiMem mem i j k
        | i == sentLen sent = (-1, 0.0)
        | otherwise = argmax eval $ labelIxs sent i
        where eval h =
                  (snd $ mem (i + 1) h j) + psiMem h
                  + onTransition crf (c h) a b
              a = interp sent (i - 1) j
              b = interp sent (i - 2) k
              c = interp sent i
    collectMaxArg (i, j, k) acc mem =
        collect $ mem i j k
        where collect (h, _)
                  | h == -1 = reverse acc
                  | otherwise = collectMaxArg (i + 1, h, j) (h:acc) mem

tag :: Model -> Sent Int Int -> [Int]
tag = dynamicTag

tagProbs :: Model -> Sent Int Int -> [[Double]]
tagProbs crf sent =
    let alpha = forward maximum crf sent
        beta = backward maximum crf sent
        normalize xs =
            let d = - logSum xs
            in map (+d) xs
        m1 k x = maximum
            [ alpha k x y + beta (k + 1) x y
            | y <- labelIxs sent (k - 1) ]
    in  [ map exp $ normalize [m1 i k | k <- labelIxs sent i]
        | i <- [0 .. sentLen sent - 1] ]

-- tag probabilities with respect to
-- marginal distributions
tagProbs' :: Model -> Sent Int Int -> [[Double]]
tagProbs' crf sent =
    let alpha = forward logSum crf sent
        beta = backward logSum crf sent
    in  [ [ exp $ prob1 crf alpha beta sent i k
          | k <- labelIxs sent i ]
        | i <- [0 .. sentLen sent - 1] ]

tag' :: Model -> Sent Int Int -> [Label]
tag' crf sent = map interp' $ zip [0..] $ tag crf sent
    where interp' (i, k) = interp sent i k

goodAndBad :: Model -> Sent Int Int -> (Int, Int)
goodAndBad crf sent =
    foldl gather (0, 0) $ zip labels labels'
    where labels = [ fst $ maximumBy (compare `on` snd)
                         $ labelProbs w
                   | w <- words sent ]
          labels' = tag' crf sent
          gather (good, bad) (x, y)
              | x == y = (good + 1, bad)
              | otherwise = (good, bad + 1)

goodAndBad' :: Model -> [Sent Int Int] -> (Int, Int)
goodAndBad' crf dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  foldl add (0, 0) $ map (goodAndBad crf) dataset

accuracy :: Model -> [Sent Int Int] -> Double
accuracy crf dataset = fromIntegral good / fromIntegral (good + bad)
    where (good, bad) = goodAndBad' crf dataset

-- parallel implementation
accuracy' :: Int -> Model -> [Sent Int Int] -> Double
accuracy' k crf dataset =
    let parts = partition k dataset
        xs = parMap rseq (goodAndBad' crf) parts
        (good, bad) = foldl add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)

-- --------------------------------------------------------------

-- prob :: V.Vect t Int => Model -> Sent Int t -> Double
-- prob crf sent =
--     sum [ phiOn crf sent k
--         | k <- [0 .. (length sent) - 1] ]
--     - zx' crf sent
-- 
-- -- TODO: Wziac pod uwage "Regularization Variance" !
-- cll :: Model -> [Sentence] -> Double
-- cll crf dataset = sum [prob crf sent | sent <- dataset]

prob3 :: Model -> ProbArray -> ProbArray -> Sent Int Int
      -> Int -> Int -> Int -> Int -> Double
prob3 crf alpha beta sent k x y z =
    alpha (k - 1) y z + beta (k + 1) x y
    + phi crf (obvs $ word sent k) a b c
    - zxBeta beta
    where a = interp sent k       x
          b = interp sent (k - 1) y
          c = interp sent (k - 2) z

prob3' crf alpha beta sent k psiMem x y z =
    alpha (k - 1) y z + beta (k + 1) x y + psiMem x
    + onTransition crf a b c - zxBeta beta
    where a = interp sent k       x
          b = interp sent (k - 1) y
          c = interp sent (k - 2) z

prob2 :: Model -> ProbArray -> ProbArray
      -> Sent o t -> Int -> Int -> Int -> Double
prob2 crf alpha beta sent k x y =
    alpha k x y + beta (k + 1) x y - zxBeta beta

prob2' :: Model -> ProbArray -> ProbArray -> Sent Int Int
       -> Int -> Int -> Int -> Double
prob2' crf alpha beta sent k x y = logSum
    [ prob3 crf alpha beta sent k x y z
    | z <- labelIxs sent (k - 2) ]

prob1 :: Model -> ProbArray -> ProbArray -> Sent Int Int
      -> Int -> Int -> Double
prob1 crf alpha beta sent k x = logSum
    [ prob2 crf alpha beta sent k x y
    | y <- labelIxs sent (k - 1) ]

expectedFeaturesOn :: Model -> ProbArray -> ProbArray -> Sent Int Int
                   -> Int -> [(Feature, Double)]
expectedFeaturesOn crf alpha beta sent k =
    fs3 ++ fs1 -- `using` parList evalElem
    where psiMem = computePsiMem crf sent k
          pr1 = prob1  crf alpha beta sent k
          pr3 = prob3' crf alpha beta sent k psiMem
          fs1 = [ (feature, pr1 a) 
                | a <- labelIxs sent k
                , feature <- observationFeaturesFor' sent k a ]
    	  fs3 = [ (feature, pr3 a b c) 
                | (a, b, c) <- labelIxs3 sent k
                , feature <- transitionFeaturesFor' sent k (a, b, c) ]

expectedFeaturesIn :: Model -> Sent Int Int -> [(Feature, Double)]
expectedFeaturesIn crf sent =
    -- force parallel computation of alpha and beta tables
    zx1 `par` zx2 `pseq` zx1 `pseq` concat
    ( [ expectedFeaturesOn crf alpha beta sent k
      | k <- [0 .. sentLen sent - 1] ]
      -- parallel computation on different positions
      `using` parList (evalList evalElem) )
    where alpha = forward logSum crf sent
          beta = backward logSum crf sent
          zx1 = zxAlpha maximum sent alpha
          zx2 = zxBeta beta
          evalElem = evalTuple2 rseq rseq
