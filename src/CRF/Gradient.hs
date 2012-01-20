module CRF.Gradient
( computeGradient
, applyGradient
, Gradient
) where

import CRF.Base
import CRF.LogMath (logAdd)
import CRF.Feature (featuresIn)
import CRF.Model (Model, expectedFeaturesIn)
import qualified CRF.Model.Internal as MI
import qualified CRF.Data.MarkedArray as MA

type Gradient = MA.MarkedArray

computeGradient :: Model -> Double -> Gradient -> [Sent Int Int] -> IO Gradient
computeGradient crf scale buffer part =
    let ns = concat $ map featuresIn part
        ens = concat $ map (expectedFeaturesIn crf) part
    	followPtrs = map (\(feat, val) -> (MI.featToIx feat crf, val))
    in do
        gradient <- MA.consumeWith logAdd (followPtrs ens) buffer
                >>= MA.mapArray (\v -> - exp v) 
                >>= MA.consumeWith (+) (followPtrs ns)
                >>= MA.mapArray (* scale)
        return gradient

applyGradient :: Gradient -> Model -> IO Model
applyGradient grad crf =
    MA.elems grad >>= \xs -> MI.consumeWith (+) xs crf
