module CRF.Model.Internal
( Model
, fromList
, toList
--, Student (..)
, makeModel
--, makeStudent
-- , showModel
-- , readModel
, modelSize
, onOFeat
, onTFeat
, featToIx
, consumeWith
, mapModel
, mapModel'
-- , updateWithNumbers
--, gradientInternal
--, applyGradients
) where

import Data.Maybe (fromJust)
import Control.Monad (forM_)
import qualified Data.Array.IO as A
import qualified Data.Array.MArray as A
import qualified Data.Array.Unboxed as A
import qualified Data.IntMap as IM
import qualified Data.Set as Set
import Data.List (foldl', zip3, zip4, zip5)
import Data.Binary

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)

import qualified CRF.Data.MarkedArray as MA
import CRF.Base
import CRF.Feature
import CRF.LogMath
import CRF.Util (partition)

import Debug.Trace (trace)

type Level = Int
type Observation = Int

data Model = Model
    { observationIxs :: IM.IntMap (IM.IntMap Int)
    , transitionIxs :: IM.IntMap Int
    , values :: A.UArray Int Double
    , labelMax :: Int }

instance Binary Model where
    put crf = put ( observationIxs crf
                  , transitionIxs crf
                  , values crf
                  , labelMax crf )
    get = do
        (observationIxs, transitionIxs, values, labelMax) <- get
        return Model { observationIxs = observationIxs
                     , transitionIxs = transitionIxs
                     , values = values
                     , labelMax = labelMax }

instance Show Model where
    show = unlines . map show . toList

-- instance Read Model where
--     read = fromList . map read' . lines where
--         read' s =
--             let (feat, val) = read s
--             in feat `seq` val `seq` (feat, val)

toList :: Model -> [(Feature, Double)]
toList crf =
    [ (TFeature k x y z, v)
    | ((k, x, y, z), v) <- map dcdT $ IM.toList $ transitionIxs crf ]
    ++
    [ (OFeature k o x, v)
    | (o, om) <- IM.toList $ observationIxs crf
    , ((k, x), v) <- map dcdO $ IM.toList om ]
    where
        dcdT = dcd decodeTFeat
        dcdO = dcd decodeOFeat
        dcd with (key, ix) =
            (with (labelMax crf) key, values crf A.! ix)

fromList :: [(Feature, Double)] -> Model
fromList fs = {-# SCC "modelFromList" #-}
    let featSublabels (OFeature _ _ x) = [x]
        featSublabels (TFeature _ x y z) = [x, y, z]
        featObvs (OFeature _ o _) = [o]
        featObvs _ = []
    
        lmax = maximum $ concat $ map featSublabels $ map fst fs
        omax = maximum $ concat $ map featObvs $ map fst fs

        trsFeats = [feat | (feat, val) <- fs, isTransFeat feat]
        obsFeats = [feat | (feat, val) <- fs, isObsFeat feat]

        transitionIxs = IM.fromList $ zip
            [encodeTFeat lmax k x y z | TFeature k x y z <- trsFeats]
            [0 ..]

        observationIxs = IM.fromList
            [ (o, IM.fromList
                [ (encodeOFeat lmax k x, ix)
                | (OFeature k _ x, ix) <- fs ])
            | (o, fs) <- groupObsFeatures obsFeats' ]
            where obsFeats' = zip obsFeats [ length trsFeats .. ]

        groupObsFeatures fs = IM.toList $
            IM.fromListWith (++) 
                [ (o, [(feat, ix)])
                | (feat@(OFeature _ o _), ix) <- fs ]

        values =
            A.array (0, length fs - 1)
            [(i, 0.0) | i <- [0 .. length fs - 1]]
            A.//
            [(featToIx feat model, val) | (feat, val) <- fs]

        model = Model
            { labelMax = lmax
            , values = values
            , transitionIxs = transitionIxs
            , observationIxs = observationIxs }
    in  model

modelSize :: Model -> Int
modelSize crf =
--     snd $ A.bounds $ values crf
    (IM.size $ transitionIxs crf) +
    (sum $ map IM.size $ IM.elems $ observationIxs crf)

-- type Student = (Model, MA.MarkedArray)
--data Student = Student
--    { learned :: Model
--    , gradBufs :: [Gradient] }

makeModel :: [Feature] -> Model
makeModel fs = fromList [(feat, 0.0) | feat <- fs']
    where fs' = Set.toList $ Set.fromList fs

--makeStudent :: Model -> Int -> Student
--makeStudent crf k = Student
--    { learned = crf
--    , gradBufs = replicate k
--            $ MA.new
--            $ modelSize crf }

-- !TODO!: rozwazyc, co gdy numer etykiety wykracza
-- poza zakres !! dodac Maybe ? Wczesniej rozwiazaniem
-- bylo m = lmax + 3 !!
-- return (k, xs); should satisfy:
-- (k, xs) = decode lmax $ encode lmax k xs
-- encode :: Int -> Int -> [Int] -> Int
-- encode lmax k xs =
--     foldl' (\acc x -> acc * m + x) k xs'
--     where
--         xs' = take 3 $ [x + 1 | x <- xs] ++ [0, 0 ..]
--         m = lmax + 2

-- ASSUMPTION: 0 <= x, y, z <= lmax
encodeTFeat :: Int -> Level -> Int -> Int -> Int -> Int
encodeTFeat lmax k x y z =
    if any (== unknown) [x, y, z]
        then -1
        else foldl' (\acc x -> acc * m + x) k [x, y, z]
    where m = lmax + 1

encodeOFeat :: Int -> Level -> Int -> Int
encodeOFeat lmax k x =
    if x == unknown
        then -1
        else k * m + x
    where m = lmax + 1

decodeTFeat :: Int -> Int -> (Int, Int, Int, Int)
decodeTFeat lmax n = (k, x, y, z) where
    (k, [x, y, z]) = it !! 3
    it = iterate f (n, [])
    f (n, acc) = (n `div` m, (n `mod` m):acc)
    m = lmax + 1

decodeOFeat :: Int -> Int -> (Int, Int)
decodeOFeat lmax n = (n `div` m, n `mod` m)
    where m = lmax + 1

oFeatToIx :: Level -> Observation -> Int -> Model -> Maybe Int
oFeatToIx k o x crf = do
    obsMap <- IM.lookup o $ observationIxs crf
    IM.lookup (encodeOFeat lmax k x) obsMap
    where lmax = labelMax crf

tFeatToIx :: Level -> Int -> Int -> Int -> Model -> Maybe Int
tFeatToIx k x y z crf =
    IM.lookup (encodeTFeat lmax k x y z) $ transitionIxs crf
    where lmax = labelMax crf

featToIx :: Feature -> Model -> Int
featToIx feat crf = fromJust $ case feat of
    TFeature k x y z -> tFeatToIx k x y z crf
    OFeature k o x -> oFeatToIx k o x crf

consumeWith :: (Double -> Double -> Double) -> [(Int, Double)]
            -> Model -> IO Model
consumeWith f xs crf = do
    arr <- A.unsafeThaw $ values crf
        :: IO (A.IOUArray Int Double)
    forM_ xs $ \(i, v) -> do
        w <- A.readArray arr i
        A.writeArray arr i $! f w v
    arr' <- A.unsafeFreeze arr
    return Model
        { labelMax = labelMax crf
        , values = arr'
        , transitionIxs = transitionIxs crf
        , observationIxs = observationIxs crf }

onOFeat :: Level -> Observation -> Int -> Model -> Double
onOFeat k o x crf =
    case oFeatToIx k o x crf of
        Just ix -> values crf A.! ix
        Nothing -> 0.0

onTFeat :: Level -> Int -> Int -> Int -> Model -> Double
onTFeat k x y z crf =
    case tFeatToIx k x y z crf of
        Just ix -> values crf A.! ix
        Nothing -> 0.0

mapModel :: (Double -> Double) -> Model -> IO Model
mapModel f crf = do
    arr <- A.unsafeThaw $ values crf
        :: IO (A.IOUArray Int Double)
    n <- return $ snd $ A.bounds $ values crf
    forM_ [0 .. n - 1] $
        \i -> A.writeArray arr i . f =<< A.readArray arr i
    arr' <- A.unsafeFreeze arr
    return Model
        { labelMax = labelMax crf
        , transitionIxs = transitionIxs crf
        , observationIxs = observationIxs crf
        , values = arr' }

-- parallel version
mapModel' :: Int -> (Double -> Double) -> Model -> IO Model
mapModel' k f crf = do
    let n = snd $ A.bounds $ values crf
        ps = (n `div` k) + 1
        uppers = [i * ps | i <- [1 .. k - 1]] ++ [n]
        bounds = zip (0 : uppers) uppers
        
    arr <- A.unsafeThaw $ values crf
        :: IO (A.IOUArray Int Double)

    com <- newEmptyMVar
    forM_ bounds $ \(p, q) -> forkIO $ do  
        forM_ [p .. q - 1] $
            \i -> A.writeArray arr i . f =<< A.readArray arr i
        putMVar com ()
    sequence [takeMVar com | _ <- bounds]
        
--    forM_ [0 .. n - 1] $
--        \i -> A.writeArray arr i . f =<< A.readArray arr i

    arr' <- A.unsafeFreeze arr
    return Model
        { labelMax = labelMax crf
        , transitionIxs = transitionIxs crf
        , observationIxs = observationIxs crf
        , values = arr' }
