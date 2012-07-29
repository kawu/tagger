module Text.Linc.Lexer
( lexer
, ioLexer
, Token
, TokParser
, keyword
, symbol
, quoted
, tag
, float
) where

import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Text
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (forM_)

import Text.Tagset.Data (Tagset, Label)
import Text.Tagset.TagParser (tagParser)
import Text.Tagset.DefParser (parseTagset)

--type Parser = Parsec T.Text ()
type TokParser = Parsec [Token] ()

type Token = (SourcePos, Tok)
data Tok = Keyword T.Text
         | Symbol Char
         | Quoted T.Text
         | Tag Label
         | Float Double
         deriving (Show)

(<<) :: Monad m => m a -> m b -> m a
(<<) x y = do {v <- x; y; return v}

-- The lexer

lex_keywords = ["Sent", "Nps"]
lex_symbols = ['*', '-']

lex_tokens tagset = do
    toks <- many $ lex_token tagset
    eof
    return $ catMaybes toks

--lex_token :: Tagset -> Parser (Maybe Token)
lex_token tagset = do
    pos <- getPosition
    tok <- lex_tok tagset
    return $ case tok of
        Nothing -> Nothing
        Just x -> Just (pos, x)

--lex_tok :: Tagset -> Parser (Maybe Tok)
lex_tok tagset = (many1 space >> return Nothing) 
    <|> (lex_quoted >>= return . Just . Quoted . T.pack)
    <|> ((foldl1 (<|>) $ map (try . char) lex_symbols)
        >>= return . Just . Symbol)
    <|> ((foldl1 (<|>) $ map (try . string) lex_keywords)
        >>= return . Just . Keyword . T.pack)
    <|> (tagParser tagset >>= return . Just . Tag)
    <|> (lex_float >>= return . Just . Float)

lex_quoted = (char '"' >> many lex_quotedChar) << char '"'
lex_quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

lex_float = do
    first <- digit
    rest <- many1 $ oneOf $ '.' : ['0'..'9']
    return (read $ first : rest)

lexer tagset srcName = parse (lex_tokens tagset) srcName

-- Token parsers

keyword = t_token . isKeyword
symbol = t_token . isSymbol
tag = t_token isTag
quoted = t_token isQuoted
float = t_token isFloat

isKeyword name (Keyword s)
    | s == name' = Just name'
    | otherwise = Nothing 
    where name' = T.pack name
isKeyword _ _ = Nothing

isSymbol name (Symbol c)
    | c == name = Just name
    | otherwise = Nothing 
isSymbol _ _ = Nothing

isQuoted (Quoted q) = Just q
isQuoted _ = Nothing

isTag (Tag t) = Just t
isTag _ = Nothing

isFloat (Float x) = Just x
isFloat _ = Nothing

t_token :: (Tok -> Maybe a) -> TokParser a
t_token test = token showToken posToken testToken where
    showToken (pos, tok)   = show tok
    posToken  (pos, tok)   = pos
    testToken (pos, tok)   = test tok

-- For testing

ioLexer :: Tagset -> String -> T.Text -> IO [Token]
ioLexer tagset srcName input =
    case lexer tagset srcName input of
        Left e -> fail $ "Error scanning input:\n" ++ show e
        Right r -> return r

main = do
    [tagsetPath] <- getArgs
    tagset <- parseTagset tagsetPath =<< readFile tagsetPath
    toks <- ioLexer tagset "(stdin)" =<< T.getContents
    forM_ (map snd toks) $ print 
