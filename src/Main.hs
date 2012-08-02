{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import System.Console.CmdArgs
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import System.Directory (getDirectoryContents)
import System.Random (randoms, mkStdGen)
import Data.List (isSuffixOf, maximumBy)
import Data.Ord (comparing)
import Control.Applicative ((<$>))
import Control.Monad (forM_, forM)
import Control.Monad.Lazy (mapM', forM')
import Data.Binary (encodeFile, decodeFile)
import qualified Data.Vector as V

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified CRF.Base as C
import qualified CRF.Model as C
import qualified CRF.Train as C
import qualified CRF.Feature as C
import qualified CRF.Alphabet as C
import CRF.Util (partition)

import qualified Data.Schema as Ox
import qualified Data.Feature as Ox

import qualified Schema as S

import qualified Data.Morphosyntax as M
import Text.Morphosyntax.Tagset (parseTagset)
import Text.Morphosyntax.Plain (parsePlain, showPlain)

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
schema :: S.Schema
schema sent = \k ->
    [ Ox.orth sent  k ]
--     [ Ox.orth sent (k-1)
--     , Ox.orth sent  k
--     , Ox.orth sent (k+1) ]

tiers :: [S.Tier]
tiers = S.mkTiers True $ map (S.mkTierDesc "pos")
    [["pos", "cas", "per"]]
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

data Args =
    TrainMode
        { trainPath   :: FilePath
        , evalPath    :: FilePath
        , tagsetPath  :: FilePath
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
    deriving (Data, Typeable, Show)

trainMode = TrainMode
    { tagsetPath  = def &= argPos 0 &= typ "TAGSET"
    , trainPath = def &= argPos 1 &= typ "TRAIN-DIR"
    , evalPath = def &= typDir &= help "Eval data directory"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , scale0 = 1.0 &= help "Initial scale parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , workersNum = 1 &= help "Number of gradient-computing workers"
    , outModel = def &= typFile &= help "Output model file" }

tagMode = TagMode
    { inModel = def &= argPos 0 &= typ "MODEL"
    , dataPath = Nothing
        &= help "Input file; if not specified, read from stdin"
    , printProbs = False
        &= help ("Print probabilities of labeles instead of" ++
                 " performing disambiguation") }
 
argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, tagMode]

main = do
    args <- cmdArgsRun argModes
    exec args

exec args@TrainMode{..} = do
    hSetBuffering stdout NoBuffering

    tagset <- parseTagset tagsetPath <$> readFile tagsetPath

    let readData path = parsePlain tagset <$> L.readFile path
    let schematize = S.schematize tiers schema
    let schemaTrain = map schematize <$> readData trainPath

    codec <- codecFrom tagset tiers schema trainPath
    trainData <- V.fromList <$> map (C.encodeSent codec) <$> schemaTrain
    evalData <- if null evalPath
        then return Nothing
        else Just . V.fromList <$> map (C.encodeSent codec) <$>
             map schematize <$> readData evalPath

    crf <- C.trainModel trainData evalData
        C.TrainArgs { batchSize = batchSize
                    , regVar = regVar
                    , iterNum = iterNum
                    , scale0 = scale0
                    , tau = tau
                    , workersNum = workersNum }

    if not $ null outModel
        then do
            putStr $ "\nSaving model in " ++ outModel ++ "..."
            encodeFile outModel (crf, codec, tagset)
            putStrLn ""
        else
            return ()

exec args@TagMode{..} = do
    model <- decodeFile inModel
    let (crf, codec, tagset) = model
    plain <- parsePlain tagset <$>
        case dataPath of
            Nothing -> L.getContents
            Just path -> L.readFile path
    let doTag sent = tagSent crf codec tiers (map fst sent)
    L.putStr $ showPlain tagset $ map doTag plain

codecFrom :: M.Tagset -> [S.Tier] -> S.Schema -> FilePath -> IO C.Alphabet
codecFrom tagset tiers schema path = do
    plain <- parsePlain tagset <$> L.readFile path 
    let words = concatMap (C.words . S.schematize tiers schema) plain
    let codec = C.fromWords words (length tiers)
    return $ codec `seq` codec

tagSent :: C.Model -> C.Alphabet -> [S.Tier] -> M.Sent -> M.SentMlt
tagSent crf codec tiers sent =
    [ ( word, addProbs
        [ interp
        | interp <- M.interps word
        , S.selectAll tiers (M.tag interp) == label ] )
    | (word, label) <- zip sent choices ]
  where
    encoded = C.encodeSent codec (S.schematize' tiers schema sent)
    choices = map (C.decodeL codec) (C.tag' crf encoded)
    addProbs xs =
        let pr = 1 / fromIntegral (length xs)
        in  [(x, pr) | x <- xs]
