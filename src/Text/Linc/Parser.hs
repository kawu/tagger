module Text.Linc.Parser
( parseSent
, parseLinc
) where 

import System.Environment (getArgs)
import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (groupBy)
import Control.Monad (fail)
import Control.Monad.Lazy (sequence')

import Text.Data (LincWord(LincWord), LincSent, orth, nps, interps, probs)
import Text.Linc.Lexer (ioLexer, keyword, symbol, quoted, tag, float)
import Text.Linc.Printer (printLinc)
import Text.Tagset.Data (Tagset)
import Text.Tagset.DefParser (parseTagset)

(<<) :: Monad m => m a -> m b -> m a
(<<) x y = do {v <- x; y; return v}

normalize :: RealFloat r => [r] -> [r]
normalize xs =
    coef `seq` map (/coef) xs
    where coef = sum xs

lincFile = many lincSent << eof

lincSent = keyword "Sent" >> many1 lincWord

lincWord = do
    orth <- quoted
    nps <- optionMaybe (keyword "Nps") >>=
        \x -> return $ case x of
	    Just _ -> True
	    Nothing -> False
    (interps, probs) <- many1 lincInterp
                    >>= return . unzip
    return $ LincWord { orth=orth
                      , nps=nps
                      , interps=interps
		      , probs=normalize probs}

lincInterp = do
    prob <- lincDisamb <|> float
    interp <- tag
    return (interp, prob)

lincDisamb = (symbol '*' >> return 1.0)
         <|> (symbol '-' >> return 0.0)

parseSent :: Tagset -> String -> T.Text -> IO [LincWord]
parseSent tagset source input = do
    toks <- ioLexer tagset source input
    case parse lincSent source toks of
        Left e -> fail $ "Error parsing input:\n" ++ show e
        Right r -> return r

parseLinc :: Tagset -> T.Text -> IO [[LincWord]]
parseLinc tagset input =
    sequence' [parseSent tagset (info i) sent | (i, sent) <- zip [1..] ss]
    where ss = map (T.concat . map addEoc) groups
          groups = groupBy (\_ y -> not $ sentBeg y) lines'
          lines' = dropWhile (not . sentBeg) $ T.lines input
          info i = "(sentence " ++ show i ++ ")"
          sentBeg x = T.pack "Sent" `T.isPrefixOf` x
          addEoc s = T.append s $ T.pack "\n"

main = do
    [tagsetPath] <- getArgs
    tagset <- parseTagset tagsetPath =<< readFile tagsetPath
    linc <- parseLinc tagset =<< T.getContents
    printLinc tagset True linc
