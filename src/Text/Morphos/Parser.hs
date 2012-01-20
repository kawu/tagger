module Text.Morphos.Parser
( parseMorph
) where 

import System.IO
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Array as A
import Data.List (groupBy)
import Data.Function (on)

import Text.Morphos.Data

-- import qualified Data.Text.Lazy.IO as LT
-- import Text.Morphos.Printer
-- import Control.Monad (forM_)
-- import Debug.Trace (trace)

-- TODO: error handling

type Table = [A.Array Int T.Text]

mkTable :: T.Text -> LT.Text -> Table
mkTable sep input =
    [ mkArray $ T.splitOn sep $ LT.toStrict line
    | line <- LT.lines input ]
    where mkArray xs = A.listArray (0, length xs - 1) xs

-- | WARNING: memory linear with respect to length of groups
groupOn :: Int -> Table -> [Table]
groupOn k = groupBy ((==) `on` (A.! k))

groupOn' k tab =
    [ (read $ T.unpack $ head group A.! k, group)
    | group <- groupOn k tab ]

parseMorph :: Handle -> LT.Text -> [MorphSent]
parseMorph errHandle input =
    [ MorphSent
        { docId  = read $ T.unpack $ head group A.! 0
        , sentId = read $ T.unpack $ head group A.! 1
        , sentWords = parseSent group }
    | group <- groupOn 1 tab ]
    where tab = mkTable (T.singleton '#') input

parseSent :: Table -> [MorphWord]
parseSent tab =
    [ parseWord group
    | group <- groupOn 2 tab ]

parseWord :: Table -> MorphWord
parseWord tab =
    let first = head tab 
        wid = read $ T.unpack $ first A.! 2
        orth = first A.! 3
        interps =
            [ (elem A.! 4, elem A.! 5)
            | elem <- tab ]
        probs = [0.0 | _ <- tab]
    in MorphWord
        { wid = wid
        , orth = orth
        , nps = True
        , interps = interps
        , probs = probs }

-- main = do
--     txt <- LT.getContents
--     morph <- return $ parseMorph stderr txt
--     forM_ morph printSent 
