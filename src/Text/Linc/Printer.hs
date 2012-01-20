module Text.Linc.Printer
( printLinc
, printSent
) where 

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (forM_)
import Data.List (sortBy)
import Text.Printf

import Text.Data
import Text.Tagset.Data (Tagset, Label)
import Text.Tagset.TagPrinter (showTag)

printLinc :: Tagset -> Bool -> [LincSent] -> IO ()
printLinc tagset printProbs =
    mapM_ $ printSent tagset printProbs

printSent :: Tagset -> Bool -> LincSent -> IO ()
printSent tagset ppr words = putStrLn "Sent"
    >> forM_ words (printWord tagset ppr)

printWord :: Tagset -> Bool -> LincWord -> IO ()
printWord tagset ppr word = do
    putStr "  "
    T.putStr $ escape $ orth word
    if nps word
        then putStrLn " Nps"
	else putStrLn ""
    printTags tagset ppr word

printTags :: Tagset -> Bool -> LincWord -> IO()
printTags tagset False word = do
    let disamb = snd $ maximum
               $ zip (probs word) (interps word)
    forM_ (interps word) $ \x -> do
        case x == disamb of
            True  -> putStr "    * "
            False -> putStr "    - "
        T.putStrLn $ showTag tagset x

printTags tagset True word = do
    forM_ 
        ( sortBy (\(x, p1) (y, p2) -> compare p2 p1)
          (zip (interps word) (probs word)) )
        $ \(x, prob) -> do
            putStr "    "
            printf "%.4f" $ prob
            putStr " "
            -- T.putStrLn $ T.intercalate (T.singleton ':') $ map snd x
            T.putStrLn $ showTag tagset x

escape :: T.Text -> T.Text
escape xs = T.pack "\"" `T.append` escape' xs `T.append` T.pack "\""
    where escape' = T.replace (T.singleton '"') (T.pack "\"\"")
