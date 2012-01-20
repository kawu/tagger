module Text.Data
( LincSent
, LincWord(LincWord)
, orth
, nps
, interps
, probs
, evalLincWord
) where 

import Control.Parallel.Strategies

import Data.Text (Text)
import Text.Tagset.Data (Label)

type LincSent = [LincWord]
data LincWord = LincWord { orth :: Text
                         , nps :: Bool
                         , interps :: [Label]
                         , probs :: [Double]}

evalLincWord :: Strategy LincWord
evalLincWord word = do
    orth <- rseq $ orth word
    nps <- rseq $ nps word
    interps <- evalList (evalList $ evalTuple2 rseq rseq)
             $ interps word
    probs <- evalList rseq $ probs word
    return LincWord 
            { orth = orth
             , nps = nps
             , interps = interps
             , probs = probs }
