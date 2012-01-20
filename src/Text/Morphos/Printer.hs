module Text.Morphos.Printer
( printSent
) where 

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import Text.Printf

import Text.Morphos.Data

-- printDoc :: MorphDoc -> IO ()
-- printDoc (docId, docData) =
--     forM_ docData $ printSent docId

printSent msent =
    forM_ (sentWords msent) $ printWord (docId msent) (sentId msent)

printWord docId sentId word =
    forM_ (zip (interps word) (probs word))
        $ printLine docId sentId word 

printLine docId sentId word ((lemma, msd), prob) = do
    putStr $ L.intercalate "#" 
        [ show docId
        , show sentId
        , show $ wid word ]
    putChar '#'
    T.putStr $ T.intercalate (T.singleton '#') [orth word, lemma, msd]
    printf "#%.4f#\n" $ prob
