{-# LANGUAGE DeriveDataTypeable
           , ScopedTypeVariables
           , RecordWildCards #-}

import System.Console.CmdArgs
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import System.Directory (getDirectoryContents)
import System.Random (randoms, mkStdGen)
import Data.List (isSuffixOf)
import Control.Applicative ((<$>))
import Control.Monad (forM_, forM)
import Control.Monad.Lazy (mapM', forM')
import Data.Binary (encodeFile, decodeFile)

import Control.Parallel.Strategies ( using, parList, parBuffer, evalList
                                   , evalTuple2, rseq, parMap )

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import qualified CRF.Base as C
import qualified CRF.Model as C
import qualified CRF.Train as C
import qualified CRF.Feature as C
import qualified CRF.Alphabet as C
import CRF.Util (partition)

-- import qualified CRF.Data.DataSet as DS
-- import qualified Data.LincPathDS as DS
import Data.LincInMemory (lincInMemory)

import qualified Text.Data as L
import qualified Text.Plain as Plain

import qualified Text.Tagset.Data as TS
import qualified Text.Tagset.TagParser as TS
import qualified Text.Tagset.DefParser as TS

import qualified Schema as S

-- readDataSetPaths :: FilePath -> IO [FilePath]
-- readDataSetPaths dirName = do
--     files <- getDirectoryContents dirName
--     return $ map ((dirName ++ "/") ++) $ filter (isSuffixOf ".linc") files

data Args =
    TrainMode
        { trainPath   :: FilePath
        , evalPath    :: FilePath
        , tagsetPath  :: FilePath
        , schemaPath  :: FilePath
        , iterNum :: Double
        , batchSize :: Int
        , regVar :: Double
        , scale0 :: Double
        , tau :: Double
        , workersNum :: Int
        , outModel :: FilePath }
    |
    TagMode
        { dataPath :: Maybe FilePath
        , inModel :: FilePath
        , printProbs :: Bool }  -- ^ FIXME: it is ignored now 
--     |
--     CVMode 
--         { dataDirPath :: FilePath
--         , tagsetPath :: FilePath
--         , schemaPath :: FilePath
--         , kCrossVal :: Int
--         , iterNum :: Double
--         , batchSize :: Int
--         , regVar :: Double
--         , scale0 :: Double
--         , tau :: Double
--         , workersNum :: Int }
    deriving (Data, Typeable, Show)

trainMode = TrainMode
    { tagsetPath  = def &= argPos 0 &= typ "TAGSET"
    , schemaPath  = def &= argPos 1 &= typ "SCHEMA"
    , trainPath = def &= argPos 2 &= typ "TRAIN-DIR"
    , evalPath = def &= typDir &= help "Eval data directory"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , scale0 = 1.0 &= help "Initial scale parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , workersNum = 1 &= help "Number of gradient-computing workers"
    , outModel = def &= typFile &= help "Output model file" }

-- cvMode = CVMode
--     { tagsetPath = def &= argPos 0 &= typ "TAGSET"
--     , schemaPath = def &= argPos 1 &= typ "SCHEMA"
--     , dataDirPath = def &= argPos 2 &= typ "DATA-DIR"
--     , kCrossVal = 10 &= help "K-cross evaluation"
--     , iterNum = 10 &= help "Number of SGD iterations"
--     , batchSize = 30 &= help "Batch size"
--     , regVar = 10.0 &= help "Regularization variance"
--     , scale0 = 1.0 &= help "Initial scale parameter"
--     , tau = 5.0 &= help "Initial tau parameter"
--     , workersNum = 1 &= help "Number of gradient-computing workers" }

tagMode = TagMode
    { inModel = def &= argPos 0 &= typ "MODEL"
    , dataPath = Nothing
        &= help "Input file; if not specified, read from stdin"
    , printProbs = False
        &= help "Print probabilities of labeles instead of performing disambiguation" }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, tagMode]

main = do
    args <- cmdArgsRun argModes
    exec args

exec args@TrainMode{..} = do
    hSetBuffering stdout NoBuffering

    tagset <- TS.parseTagset tagsetPath =<< readFile tagsetPath
    schema <- S.parseSchema tagset schemaPath =<< readFile schemaPath

    (trainPart, alphabet) <- lincInMemory tagset schema Nothing trainPath

    evalPart <- if null evalPath
        then return Nothing
        else Just . fst
         <$> lincInMemory tagset schema (Just alphabet) evalPath

    crf <- C.trainModel trainPart evalPart
        C.TrainArgs { batchSize = batchSize
                    , regVar = regVar
                    , iterNum = iterNum
                    , scale0 = scale0
                    , tau = tau
                    , workersNum = workersNum }

    if not $ null outModel
        then do
            putStr $ "\nSaving model in " ++ outModel ++ "..."
            encodeFile outModel (crf, alphabet, tagset, schema)
            putStrLn ""
        else
            return ()


-- exec args@CVMode{..} = do
--     hSetBuffering stdout NoBuffering
-- 
--     tagset <- TS.parseTagset tagsetPath =<< readFile tagsetPath
--     schema <- S.parseSchema tagset schemaPath =<< readFile schemaPath
-- 
--     dataSetPaths <- readDataSetPaths dataDirPath
--     parts <- return $ partition kCrossVal dataSetPaths
-- 
--     stats <- mapM' (\i -> trainOn args i parts tagset schema) [0 .. kCrossVal - 1]
--     
--     result <- return $ (sum stats) / (fromIntegral $ length stats)
--     result `seq` putStrLn $ ("\naverage accuracy: " ++) $ show $ result

exec args@TagMode{..} = do
    model <- decodeFile inModel
    let (crf, alphabet, tagset, schema) = model
    plain <- Plain.parsePlain tagset <$>
        case dataPath of
            Nothing -> LT.getContents
            Just path -> LT.readFile path
    let tagged = map (tagSent schema crf alphabet) plain
    LT.putStr $ Plain.showPlain tagset tagged

tagSent :: [S.LayerCompiled] -> C.Model -> C.Alphabet
        -> Plain.Sentence -> Plain.Sentence
tagSent layers crf alphabet plain =
    let linc = Plain.mkLincSent plain
        sent = C.encodeSent alphabet $ S.schematizeSent layers linc
        sentProbs = C.tagProbs crf sent
    in map (uncurry Plain.applyProbs1) (zip sentProbs plain)

-- trainOn :: Args -> Int -> [[FilePath]] -> TS.Tagset -> [S.LayerCompiled] -> IO Double
-- trainOn args@CVMode{..} k parts tagset schema =
--     let evalPaths = parts !! k
--         trainPaths = concat [part | (i, part) <- zip [0..] parts, i /= k]
--     in do
--         putStrLn $ "\n  -- PART " ++ (show k) ++ " --\n"
--         (trainPart, alphabet) <-
--             DS.lincInMemory tagset schema Nothing trainPaths
--         (evalPart, _) <-
--             DS.lincInMemory tagset schema (Just alphabet) evalPaths 
--         crf <- C.trainModel trainPart (Just evalPart)
--             C.TrainArgs { batchSize = batchSize
--                         , regVar = regVar
--                         , iterNum = iterNum
--                         , scale0 = scale0
--                         , tau = tau
--                         , workersNum = workersNum }
--         return . C.accuracy' workersNum crf
--             =<< DS.readDataSet evalPart
