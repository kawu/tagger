module Schema
( Schema
, LayerCompiled
, parseSchema
, schematize
, schematizeSent
) where 

import System.Environment
import Control.Exception (throwIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import Data.Char (isSpace)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, transpose)
import Data.Maybe (catMaybes)
import Control.Monad (liftM, liftM2)
import Data.Binary (Binary, get, put, putWord8, getWord8)

import Control.Parallel (pseq)
import Control.Parallel.Strategies (parList, using, rseq)
import CRF.Control.Eval (forceList)

import Text.Data (LincWord, orth, interps, probs)
import Text.Linc.Parser (parseLinc)
import Text.Tagset.Data (Tagset, Label, Attr, allAtts, attrValues)
import Text.Tagset.DefParser (parseTagset)
import Text.Comment (removeComment)

import qualified CRF.Base as C

-- -- Word translated with respect to some Scheme; a tuple (ws, ds, is), where:
-- --  ws -- list of words, where each word consists of observations
-- --        related to a given layer,
-- --  ds -- disambiguated interpretation (di); list of parts of di divided
-- --        between layers,
-- --  is -- list of interpretations, each interpretation has the same format
-- --        as ds.
-- type SchemedWord = ( [[T.Text]]
--                    , [T.Text]
--                    , [[T.Text]] )

type SchemedWord = C.Word T.Text T.Text
type SchemedSent = C.Sent T.Text T.Text

-- Observation types
data Obv = Orth Int
         | Prefix Int Int
         | Suffix Int Int
         deriving (Show)

instance Binary Obv where
    put (Orth i)     = putWord8 0 >> put i
    put (Prefix i j) = putWord8 1 >> put i >> put j
    put (Suffix i j) = putWord8 2 >> put i >> put j
    get = do
        tag <- getWord8
        case tag of
            0 -> liftM Orth get
            1 -> liftM2 Prefix get get
            2 -> liftM2 Suffix get get

obvValue :: Obv -> [LincWord] -> Int -> Maybe T.Text
obvValue (Orth i) sent k =
    if k + i >= 0 && k + i < length sent then
        Just $ T.toLower $ orth $ sent !! (k + i)
    else
        Nothing

obvValue (Prefix h i) sent k = do
    orth <- obvValue (Orth i) sent k
    if T.length orth >= fromIntegral h then
        Just $ T.take (fromIntegral h) orth
    else
        Nothing

obvValue (Suffix h i) sent k = do
    orth <- obvValue (Orth i) sent k
    if T.length orth >= fromIntegral h then
        Just $ T.drop (T.length orth - fromIntegral h) orth
    else
        Nothing

-- Group of observations: alternative of conjunctions of observations
type ObvGroup = [[Obv]]

obvGroupValue :: ObvGroup -> [LincWord] -> Int -> [T.Text]
obvGroupValue group sent k =
    catMaybes [conjToStr $ conjValue obvConj | obvConj <- group]
    where
        conjValue = map (\obv -> obvValue obv sent k)
        conjToStr conj = processValues conj >>= return . T.intercalate ape
        -- assumption: single observation value /= ""
        processValues values = sequence values >>= return . map (escape ape)
        -- escape c = T.replace c (T.cons '\\' c)
        escape c = T.replace c (c `T.append` c)
        ape = T.singleton '@'

-- List of named groups
type ObvSet = [(String, ObvGroup)]

-- Tagging layers; corresponding tags and observation group names
data Layer = Layer { atts :: [Attr], obvGroups :: [String] } deriving (Show)

data Schema = Schema { obvSet :: ObvSet
                     , layers :: [Layer] } deriving (Show)

schemaParser = do
    spaces >> string "[observations]" >> spaces
    obvSet <- obvRuleParser `endBy` spaces
    layers <- many1 $ layerParser
    return Schema { obvSet=obvSet
                  , layers=layers}

obvRuleParser = do
    groupName <- ident
    spaces >> char '='
    exprs <- groupExprParser `sepBy` char '|'
    return (groupName, exprs)

groupExprParser = groupElemParser `sepBy` char '+'

groupElemParser = between spaces spaces $
    try orthParser <|>
    try prefParser <|>
    suffParser

orthParser = do
    k <- string "@orth" >> spaces >> integer
    return $ Orth (fromInteger k)

prefParser = do
    i <- string "@prefix" >> spaces >> integer
    k <- spaces >> integer
    return $ Prefix (fromInteger i) (fromInteger k)

suffParser = do
    i <- string "@suffix" >> spaces >> integer
    k <- spaces >> integer
    return $ Suffix (fromInteger i) (fromInteger k)

layerParser = do
    string "[layer]" >> spaces
    string "tags" >> spaces >> char '=' >> spaces
    atts <- (ident <|> string "*") `endBy` spacesNoEOL
    spaces

    string "observations" >> spaces >> char '=' >> spaces
    obvGroups <- ident `endBy` spacesNoEOL
    spaces

    return Layer {atts=atts, obvGroups=obvGroups}

spaceNoEOL = satisfy $
    \c -> (isSpace c) && (not $ isEOL c)
    where isEOL c = c `elem` "\n\r"
spacesNoEOL = many spaceNoEOL

-- identifier
ident = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "._"

integer = Token.integer $ Token.makeTokenParser haskellDef

type LayerCompiled = ( [ObvGroup]	-- observation groups
                     , Set.Set T.Text )	-- attribute names (maybe with 'pos')

interpretStar :: Tagset -> Schema -> [Layer]
interpretStar tagset schema =
    map substStar $ layers schema
    where
        missing = Set.toList $ inTagSet `Set.difference` inLayers
        inTagSet = Set.fromList $ allAtts tagset
        inLayers = Set.fromList $ concat $ map atts $ layers schema
        substStar layer =
            Layer { obvGroups = obvGroups layer
                  , atts = concat $ map starTo $ atts layer }
        starTo "*" = missing
        starTo attr = [attr]

compileSchema :: Tagset -> Schema -> [LayerCompiled]
compileSchema tagset schema =
    let oset = Map.fromList $ obvSet schema
    in  [ compileLayer tagset oset layer
        | layer <- interpretStar tagset schema ]

compileLayer :: Tagset
             -> Map.Map String ObvGroup
             -> Layer
             -> LayerCompiled
compileLayer tagSet obvSet layer =
    (obGroups, layerAtts)
    where 
        -- layerAtts = Set.fromList $ map T.pack $
        --     [val | attr <- atts layer, val <- attrValues tagSet attr]
        layerAtts = Set.fromList $ map T.pack $ atts layer
        obGroups = [obvSet Map.! groupName | groupName <- obvGroups layer]

schematize :: [LayerCompiled] -> [LincWord] -> Int -> SchemedWord
schematize schema sent k =
    C.newWord
        observations $
        zip (map processLabel $ interps word) (probs word)
    where
        word = sent !! k

        observations =
            [ concat [ groupValues i group
                     | (i, group) <- zip [0..] obvGroups ]
            | (obvGroups, _) <- schema ]
        groupValues i group =
            let values = obvGroupValue group sent k
            in map (appendNumber i) values
        appendNumber i x = T.append (T.pack $ show i) x

        processLabel interp =
            [ makeSubLabel interp layerAtts
            | (_, layerAtts) <- schema ]
        makeSubLabel attrPairs layerAtts =
            let parts = [ jointPart attr attrVal
                        | (attr, attrVal) <- attrPairs
                        , Set.member attr layerAtts ]
                jointPart name val = name `T.append` equal `T.append` val
                colon = T.pack ":"
                equal = T.pack "="
            in  T.intercalate colon parts

schematizeSent :: [LayerCompiled] -> [LincWord] -> SchemedSent
schematizeSent schema sent =
    -- force computation of schemed
    -- forceList schemed `pseq`
    C.newSent (length schema) schemed
    where schemed = [ schematize schema sent k
                    | k <- [0 .. length sent - 1] ]
                    -- delegate parallel computation
                    -- `using` parList rseq

parseSchema :: Tagset -> String -> String -> IO [LayerCompiled]
parseSchema tagset source input = do
    schema <- return $ parse schemaParser ("(" ++ source ++ ")")
            $ unlines $ map (removeComment '#') $ lines input
    case schema of
        Left e -> fail $ "Error parsing schema:\n" ++ show e
        Right r -> return $ compileSchema tagset r

main = do
    [tagsetPath, schemaPath, lincPath] <- getArgs
    tagset <- parseTagset tagsetPath =<< readFile tagsetPath
    schema <- parseSchema tagset schemaPath =<< readFile schemaPath
    linc <- parseLinc tagset =<< T.readFile lincPath
    printWith (schematizeSent schema) linc

printWith f [] = return ()
printWith f (r:xs) = (print $ f r) >> printWith f xs
