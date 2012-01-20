module Text.Morphos.Data
( MorphSent (..)
, MorphWord (..)
, toLinc
, merge
, evalMorphSent
) where 

import System.IO
import qualified Data.Text as Tx
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sort)
import Control.Parallel.Strategies

import qualified Text.Data as L

import qualified Text.Tagset.Data as T
import qualified Text.Tagset.TagParser as T

data MorphSent = MorphSent
    { docId :: ID
    , sentId :: ID
    , sentWords :: [MorphWord] }

data MorphWord = MorphWord
    { wid :: ID
    , orth :: Tx.Text
    , nps :: Bool
    , interps :: [(Lemma, Msd)]
    , probs :: [Double] }

-- | Evaluation strategy
evalMorphSent :: Strategy MorphSent
evalMorphSent sent = do
    docId <- rseq $ docId sent
    sentId <- rseq $ sentId sent
    sentWords <- evalList evalMorphWord $ sentWords sent
    return MorphSent
        { docId = docId
        , sentId = sentId
        , sentWords = sentWords }

evalMorphWord :: Strategy MorphWord
evalMorphWord word = do
    wid <- rseq $ wid word
    orth <- rseq $ orth word
    nps <- rseq $ nps word
    interps <- evalList (evalTuple2 rseq rseq)
             $ interps word
    probs <- evalList rseq $ probs word
    return MorphWord 
        { wid = wid
        , orth = orth
        , nps = nps
        , interps = interps
        , probs = probs }

type ID = Int
type Lemma = Tx.Text
type Msd = Tx.Text

toLinc :: Handle -> T.Tagset -> MorphSent -> IO (L.LincSent, [[Int]])
toLinc errHandle tagset sent = catch
    (mapM (toLincWord tagset) (sentWords sent) >>= return . unzip)
    (\e -> hPutStr errHandle (show e) >> return ([], []))

-- | The additional return value xs (of type [Int]) satisfies:
-- morphWord.interps[i] `corresponds to` lincWord.interps[xs[i]]
toLincWord :: T.Tagset -> MorphWord -> IO (L.LincWord, [Int])
toLincWord tagset mword = do
    ints <- mapM (parseTag tagset $ "word " ++ (show $ wid mword))
                 (map snd $ interps mword)
    intsUniq <- return $ S.toList $ S.fromList ints
    intsMap <- return $ M.fromList $ zip intsUniq [0..]
    refs <- return [intsMap M.! interp | interp <- ints]
    return ( L.LincWord
               { L.orth = orth mword
               , L.nps = nps mword
               , L.interps = intsUniq
               , L.probs = [0.0 | _ <- intsUniq] }
           , refs )

-- | Merge Morphos sentence with LINC sentence with given references.
merge :: MorphSent -> L.LincSent -> [[Int]] -> MorphSent
merge msent lsent refs = MorphSent
    { sentWords =
        [ mkWord mword lword wordRefs
        | (mword, lword, wordRefs)
            <- zip3 (sentWords msent) lsent refs ]
    , docId = docId msent
    , sentId = sentId msent }

mkWord mword lword wrefs =
    let (probs, ints) = unzip $ reverse $ sort
                      $ zip [ L.probs lword !! ref
                            | ref <- wrefs ] (interps mword)
        normalize xs =
            let d = sum xs
            in  map (/d) xs
    in MorphWord
        { wid = wid mword
        , orth = orth mword
        , nps = nps mword
        , interps = ints
        , probs = normalize probs }

-- | Tag parsing consists of two phases
-- * preprocessing (hand-written corrections),
-- * parsing with respect to Tagset.
parseTag :: T.Tagset -> String -> Msd -> IO T.Label
parseTag tagset source msd = T.parseTag tagset source
                           $ removeSlash
                           $ ppToPp1
                           $ pnToN
                           $ removeSpaces msd
    where
        removeSpaces = Tx.replace (Tx.singleton ' ') (Tx.pack "")
                     . Tx.replace (Tx.singleton '_') (Tx.pack "")
        pnToN = Tx.replace (Tx.pack "pn.") (Tx.pack "n.")
        ppToPp1 = Tx.replace (Tx.pack "pp") (Tx.pack "pp1")
        removeSlash = fst . Tx.breakOn (Tx.pack "/")

