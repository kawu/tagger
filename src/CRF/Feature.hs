module CRF.Feature
( Feature (TFeature, OFeature)
, isObsFeat
, isTransFeat
, transitionFeatures
, observationFeatures
, features
, featuresIn
, transitionFeatures'
, observationFeatures'
, transitionFeaturesFor'
, observationFeaturesFor'
, features'
, featuresIn'
) where

import Data.List (zip4)
-- import Control.Parallel.Strategies (parMap, evalList, rseq)

import qualified CRF.Data.Vect as V
import CRF.Base

data Feature = TFeature !Int !Int !Int !Int
             | OFeature !Int !Int !Int
	     deriving (Show, Read, Eq, Ord)

isObsFeat :: Feature -> Bool
isObsFeat (OFeature _ _ _) = True
isObsFeat _ = False

isTransFeat :: Feature -> Bool
isTransFeat = not . isObsFeat

-- features present in data together with
-- corresponding probabilities
-- TODO: consider doing computation in log scale (would have to change
--       Model.Internal.updateWithNumbers too).

transitionFeatures :: Sent o Int -> Int -> [(Feature, Double)]
transitionFeatures s k =
    [ (TFeature i x y z, px * py * pz)
    | (tx, px) <- labelProbsOn s k      , px > 0
    , (ty, py) <- labelProbsOn s (k - 1), py > 0
    , (tz, pz) <- labelProbsOn s (k - 2), pz > 0
    , (i, x, y, z) <- zip4 [0..] (V.toList tx) (V.toList ty) (V.toList tz) ]

observationFeatures :: Sent Int Int -> Int -> [(Feature, Double)]
observationFeatures s k =
    [ (OFeature i o x, px)
    | (tx, px) <- labelProbsOn s k, px > 0
    , (i, os, x) <- zip3 [0..] (obvs $ word s k) (V.toList tx)
    , o <- os ]

features :: Sent Int Int -> Int -> [(Feature, Double)]
features s k = transitionFeatures s k
            ++ observationFeatures s k

featuresIn s = concat $ map (features s) [0 .. sentLen s - 1] 
-- featuresIn s =
--     concat $ parMap evalElem (features s) [0 .. sentLen s - 1] 
--     where evalElem = evalList $ evalTuple2 rseq rseq

-- features, which could occure in data

observationFeaturesFor' :: Sent Int Int -> Int -> Int -> [Feature]
observationFeaturesFor' sent k a = 
    [ OFeature i o x 
    | (i, os, x) <- zip3 [0..]
                         (obvs $ word sent k)
                         (V.toList $ interp sent k a)
    , o <- os ]

transitionFeaturesFor' :: Sent o Int -> Int -> (Int, Int, Int) -> [Feature]
transitionFeaturesFor' sent k (a, b, c) =
    [ TFeature i x y z 
    | (i, x, y, z) <- zip4 [0..]
                           (V.toList $ interp sent k       a)
                           (V.toList $ interp sent (k - 1) b)
                           (V.toList $ interp sent (k - 2) c) ]

observationFeatures' :: Sent Int Int -> Int -> [Feature]
observationFeatures' sent k =
    concat $ map (observationFeaturesFor' sent k) $ labelIxs sent k

transitionFeatures' :: Sent o Int -> Int -> [Feature]
transitionFeatures' sent k =
    concat $ map (transitionFeaturesFor' sent k) $ labelIxs3 sent k

-- transitionFeatures' :: V.Vect t Int => Sent o t -> Int -> [Feature]
-- transitionFeatures' s k =
--     [ TFeature i x y z
--     | (tx, _) <- labelProbsOn s k     
--     , (ty, _) <- labelProbsOn s (k - 1)
--     , (tz, _) <- labelProbsOn s (k - 2)
--     , (i, x, y, z) <- zip4 [0..] (V.toList tx) (V.toList ty) (V.toList tz) ]
-- 
-- observationFeatures' :: V.Vect t Int => Sent Int t -> Int -> [Feature]
-- observationFeatures' s k =
--     [ OFeature i o x
--     | (tx, _) <- labelProbsOn s k
--     , (i, os, x) <- zip3 [0..] (obvs $ word s k) (V.toList tx)
--     , o <- os ]

features' :: Sent Int Int -> Int -> [Feature]
features' s k = transitionFeatures' s k
             ++ observationFeatures' s k

featuresIn' s = concat $ map (features' s) [0 .. sentLen s - 1] 
-- featuresIn' s =
--     concat $ parMap evalElem (features' s) [0 .. sentLen s - 1] 
--     where evalElem = evalList rseq
