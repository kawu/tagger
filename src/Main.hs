{-# LANGUAGE DeriveDataTypeable
           , ScopedTypeVariables
           , RecordWildCards #-}

import System.Console.CmdArgs
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import System.Directory (getDirectoryContents)
import System.Random (randoms, mkStdGen)
import Data.List (isSuffixOf)
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

import qualified CRF.Data.DataSet as DS
import qualified Data.LincPathDS as DS
import qualified Data.LincInMemory as DS

import qualified Text.Data as L
import qualified Text.Linc.Parser as L
import qualified Text.Linc.Printer as L

import qualified Text.Morphos.Data as M
import qualified Text.Morphos.Parser as M
import qualified Text.Morphos.Printer as M

import qualified Text.Tagset.Data as TS
import qualified Text.Tagset.DefParser as TS

import qualified Schema as S

readDataSetPaths :: FilePath -> IO [FilePath]
readDataSetPaths dirName = do
    files <- getDirectoryContents dirName
    return $ map ((dirName ++ "/") ++) $ filter (isSuffixOf ".linc") files

data Args =
    TrainMode
        { trainDirPath :: FilePath
        , evalDirPath :: FilePath
        , dataInMemory :: Bool
        , tagsetPath :: FilePath
        , schemaPath :: FilePath
        , iterNum :: Double
        , batchSize :: Int
        , regVar :: Double
        , scale0 :: Double
        , tau :: Double
        , workersNum :: Int
        , outModel :: FilePath }
    |
    TagMode
        { dataFilePath :: Maybe FilePath
        , inModel :: FilePath
        , morphosFormat :: Bool
        , printProbs :: Bool }
    |
    CVMode 
        { dataDirPath :: FilePath
        , tagsetPath :: FilePath
        , schemaPath :: FilePath
        , kCrossVal :: Int
        , iterNum :: Double
        , batchSize :: Int
        , regVar :: Double
        , scale0 :: Double
        , tau :: Double
        , workersNum :: Int }
    deriving (Data, Typeable, Show)

trainMode = TrainMode
    { tagsetPath = def &= argPos 0 &= typ "TAGSET"
    , schemaPath = def &= argPos 1 &= typ "SCHEMA"
    , trainDirPath = def &= argPos 2 &= typ "TRAIN-DIR"
    , evalDirPath = def &= typDir &= help "Eval data directory"
    , dataInMemory = False &= help "Store entire data set in memory"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , scale0 = 1.0 &= help "Initial scale parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , workersNum = 1 &= help "Number of gradient-computing workers"
    , outModel = def &= typFile &= help "Output model file" }

cvMode = CVMode
    { tagsetPath = def &= argPos 0 &= typ "TAGSET"
    , schemaPath = def &= argPos 1 &= typ "SCHEMA"
    , dataDirPath = def &= argPos 2 &= typ "DATA-DIR"
    , kCrossVal = 10 &= help "K-cross evaluation"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , scale0 = 1.0 &= help "Initial scale parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , workersNum = 1 &= help "Number of gradient-computing workers" }

tagMode = TagMode
    { inModel = def &= argPos 0 &= typ "MODEL"
    , dataFilePath = Nothing
        &= help "Input file; if not specified, read from stdin"
    , morphosFormat = False &= help "Input/output in Morphos format"
    , printProbs = False
        &= help (  "Print probabilities of labeles (the other option, "
                ++ "disambiguation, avilable only with the LINC format)" ) }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, cvMode, tagMode]

main = do
    args <- cmdArgsRun argModes
    exec args

exec args@TrainMode{..} =
    if dataInMemory
        then doTrain DS.lincInMemory args
        else doTrain DS.lincPathDS args

exec args@CVMode{..} = do
    hSetBuffering stdout NoBuffering

    tagset <- TS.parseTagset tagsetPath =<< readFile tagsetPath
    schema <- S.parseSchema tagset schemaPath =<< readFile schemaPath

    dataSetPaths <- readDataSetPaths dataDirPath
    parts <- return $ partition kCrossVal dataSetPaths

    stats <- mapM' (\i -> trainOn args i parts tagset schema) [0 .. kCrossVal - 1]
    
    result <- return $ (sum stats) / (fromIntegral $ length stats)
    result `seq` putStrLn $ ("\naverage accuracy: " ++) $ show $ result

exec args@TagMode{..} = do
    -- putStr $ "Loading model from " ++ inModel ++ "..."
    model <- return $ decodeFile inModel
    --putStr "\n"
    if morphosFormat
        then tagMorphos args model
        else tagLinc args model

doTrain mkDataSet args@TrainMode{..} = do
    hSetBuffering stdout NoBuffering

    tagset <- TS.parseTagset tagsetPath =<< readFile tagsetPath
    schema <- S.parseSchema tagset schemaPath =<< readFile schemaPath

    (trainPart, alphabet) <- readDataSetPaths trainDirPath
        >>= mkDataSet tagset schema Nothing

    evalPart <- if null evalDirPath
        then return Nothing
        else readDataSetPaths evalDirPath
            >>= mkDataSet tagset schema (Just alphabet)
            >>= return . Just . fst

    crf <- C.trainModel trainPart evalPart
        C.TrainArgs { batchSize = batchSize
                    , regVar = regVar
                    , iterNum = iterNum
                    , scale0 = scale0
                    , tau = tau
                    , workersNum = workersNum }

    if not $ null outModel then do
        putStr $ "\nSaving model in " ++ outModel ++ "..."
        encodeFile outModel (crf, alphabet, tagset, schema)
    else
        return ()

tagLinc args@TagMode{..} model = do
    (crf, alphabet, tagset, schema) <- model

    linc <- L.parseLinc tagset =<<
        case dataFilePath of
            Nothing -> T.getContents
            Just path -> T.readFile path

    tagged <- return
        ( map (tagSent schema crf alphabet) linc
          `using` parBuffer 50 (evalList L.evalLincWord) )

    mapM_ (L.printSent tagset printProbs) tagged

tagMorphos args@TagMode{..} model = do
    (crf, alphabet, tagset, schema) <- model

    morph <- return . M.parseMorph stderr =<<
        case dataFilePath of
            Nothing -> LT.getContents
            Just path -> LT.readFile path

    -- it would be nice to parallelize this too
    linc <- forM' morph $ M.toLinc stderr tagset

    tagged <- return
        ( [ M.merge msent (tagSent schema crf alphabet lsent) refs
          | (msent, (lsent, refs)) <- zip morph linc ]
          `using` parBuffer 50 M.evalMorphSent )

    mapM_ M.printSent tagged

tagSent :: [S.LayerCompiled] -> C.Model -> C.Alphabet
            -> [L.LincWord] -> [L.LincWord]
tagSent layers crf alphabet sent =
    let sent' = C.encodeSent alphabet $ S.schematizeSent layers sent
        sentProbs = C.tagProbs crf sent'
        applyProbs (word, probs) =
            L.LincWord { L.orth=orth, L.nps=nps
                       , L.interps=interps
                       , L.probs=probs }
            where orth = L.orth word
                  nps = L.nps word
                  interps = L.interps word
    in map applyProbs $ zip sent sentProbs

trainOn :: Args -> Int -> [[FilePath]] -> TS.Tagset -> [S.LayerCompiled] -> IO Double
trainOn args@CVMode{..} k parts tagset schema =
    let evalPaths = parts !! k
        trainPaths = concat [part | (i, part) <- zip [0..] parts, i /= k]
    in do
        putStrLn $ "\n  -- PART " ++ (show k) ++ " --\n"
        (trainPart, alphabet) <-
            DS.lincInMemory tagset schema Nothing trainPaths
        (evalPart, _) <-
            DS.lincInMemory tagset schema (Just alphabet) evalPaths 
        crf <- C.trainModel trainPart (Just evalPart)
            C.TrainArgs { batchSize = batchSize
                        , regVar = regVar
                        , iterNum = iterNum
                        , scale0 = scale0
                        , tau = tau
                        , workersNum = workersNum }
        return . C.accuracy' workersNum crf
            =<< DS.readDataSet evalPart
