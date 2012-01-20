module CRF.Train
( trainModel
, TrainArgs (..)
) where

import System.Random (randomRIO, setStdGen, mkStdGen)
import Control.Monad (foldM, forM_)
import Control.Monad.ST (ST, runST)

--import Control.Concurrent.ParallelIO (parallelInterleaved, stopGlobalPool)
--import Control.Parallel.Strategies (using, parList, parMap, rseq)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)

import CRF.Control.Monad.Lazy (sequence')
import qualified CRF.Data.DataSet as DS
import qualified CRF.Data.MarkedArray as MA
import CRF.Base
import CRF.Feature
import CRF.Model
import CRF.Model.Internal
import CRF.Gradient
import CRF.Util (partition)

data TrainArgs = TrainArgs
    { batchSize :: Int
    , regVar :: Double
    , iterNum :: Double
    , scale0 :: Double
    , tau :: Double
    , workersNum :: Int }

trainModel :: DS.DataSet s => s -> Maybe s -> TrainArgs -> IO Model
trainModel trainData evalData args = do
    let step :: Double
        step = fromIntegral (batchSize args)
             / fromIntegral (DS.dataSize trainData)
        scales = map (\done -> (scale0 args * tau args)
                             / (tau args + done)) [0, step ..]
        points = takeWhile (> 0.0) [iterNum args, iterNum args - step ..]

    putStr "Training data size = "
    putStrLn $ show $ DS.dataSize trainData
    case evalData of
        Nothing -> return ()
        Just eval -> do
            putStr "Evaluation data size = "
            putStrLn $ show $ DS.dataSize eval

    crf <- return . makeModel . concat . map featuresIn'
        =<< DS.readDataSet trainData  

    putStr "Model size = "
    ms <- return $ modelSize crf
    putStrLn $ show ms
    putStrLn "\n  -- TRAINING --"

    -- buffers for gradients
    gradBufs <- sequence $ replicate (workersNum args) (MA.new ms)
    setStdGen $ mkStdGen 0
    crf' <- foldM
        (trainPoint trainData evalData args step gradBufs)
        crf
        (zip points scales)
    putStrLn "\n  -- FINISHED --"

    putStrLn . ("\naccuracy train = " ++)
             . show . (accuracy' $ workersNum args) crf
        =<< DS.readDataSet trainData
    return crf'

putInfo :: DS.DataSet s => Model -> Maybe s -> Double -> TrainArgs -> IO ()
putInfo crf dataSet point args = do
    acc <- case dataSet of
        Just ds -> DS.readDataSet ds
            >>= return . show . accuracy' (workersNum args) crf
        Nothing -> return "#"
    putStrLn $ "\n" ++ "[" ++ (show $ floor $ point) ++ "] "
        ++ "accuracy eval = " ++ acc

trainPoint :: DS.DataSet ds => ds -> Maybe ds -> TrainArgs -> Double
           -> [MA.MarkedArray] -> Model -> (Double, Double) -> IO Model
trainPoint trainData evalData args step gradBufs crf (point, scale) = do
    if floor point /= floor (point - step)
        then putInfo crf evalData point args
        else putStr "."
    batch <- getBatch trainData (batchSize args)
    updateModel crf gradBufs batch args scale $ DS.dataSize trainData

updateModel :: Model -> [MA.MarkedArray] -> [Sent Int Int]
            -> TrainArgs -> Double -> Int -> IO Model
updateModel crf gradBufs batch args scale trainSize =
    let regularization v = v * regCoef
        regCoef = 1.0 - iVar2 * coef * scale
        coef = (fromIntegral $ batchSize args)
             / (fromIntegral trainSize)
        iVar2 = 1.0 / (regVar args ^ 2)

        parts = partition (workersNum args) batch
        cg (gradBuf, part) = computeGradient crf scale gradBuf part
    in do
        -- explicit concurrent computation of gradients
        com <- newEmptyMVar
        forM_ (zip gradBufs parts) $ \(buffer, part) -> forkIO $ do
            gradient <- computeGradient crf scale buffer part
            putMVar com gradient
        grads <- sequence [takeMVar com | _ <- [1..workersNum args]]

        crf' <- mapModel' (workersNum args) regularization crf
        --crf' <- mapModel regularization crf
        result <- foldM (\crf grad ->
            applyGradient grad crf) crf' grads
        forM_ grads MA.clear
        return result

getBatch :: DS.DataSet s => s -> Int -> IO [Sent Int Int]
getBatch dataSet k = do
    return (replicate k $ randomRIO (0, DS.dataSize dataSet - 1))
    >>= sequence >>= DS.readDataElems dataSet
