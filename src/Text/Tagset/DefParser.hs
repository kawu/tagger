module Text.Tagset.DefParser
( parseTagset
) where

import qualified Data.Map as Map
import Text.Parsec
import Data.Char (isSpace)

import Text.Comment (removeComment)
import Text.Tagset.Data ( Tagset(Tagset), Rule(Rule), ruleDefs, attrDefs
                        , tagView, TagView(NKJPView, MorphosView) )

(<<) :: Monad m => m a -> m b -> m a
(<<) x y = do {v <- x; y; return v}

tagsetFile = do
    spaces
    tagView <- viewSec
    attrDefs <- attrSec
    ruleDefs <- ruleSec
    return $ Tagset { tagView = tagView, ruleDefs = ruleDefs, attrDefs = attrDefs }

viewSec = do
    string "view" >> spaces >> char '=' >> spaces
    view <- (string "NKJP" >> return NKJPView)
        <|> (string "Morphos" >> return MorphosView)
    spaces >> return view

attrSec = do
    secName "ATTR" >> spaces
    defs <- attrLine `endBy` spaces
    return $ Map.fromList defs

attrLine = do
    attr <- ident
    spaces >> char '=' >> lineSpaces
    values <- ident `endBy` lineSpaces
    return (attr, values)

ruleSec = do
    secName "RULE" >> spaces
    fmap concat $ many1 ruleSubSec

ruleSubSec = do
    predAtts <- subSecAtts << spaces
    ruleLine predAtts `endBy` spaces

ruleLine predAtts = do
    predValues <- sequence [ident << lineSpaces | _ <- predAtts]
    char '=' >> lineSpaces
    actionAtts <- attrName `endBy` lineSpaces
    return $ Rule (zip predAtts predValues) actionAtts

attrName = optionalAttrName <|> plainAttrName <?> "attribute name"
optionalAttrName = do
    char '['
    name <- ident
    char ']'
    return (name, True)
plainAttrName = do
    name <- ident
    return $ (name, False)

lineSpace = satisfy $ \c -> (isSpace c) && (not $ c == '\n')
lineSpaces = many lineSpace

ident = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "."

secName name = char '[' >> string name >> char ']'
subSecAtts = do 
    char '{'
    atts <- ident `endBy` lineSpaces
    char '}'
    return atts

-- eolChar = char '\n'

parseTagset srcName contents = do
    parseResult <- return $ parse tagsetFile srcName
                 $ unlines $ map (removeComment '#')
                 $ lines contents
    case parseResult of
        Left e -> fail $ "Error parsing input:\n" ++ show e
        Right r -> return r

main = do
    c <- getContents
    tagset <- parseTagset "(stdin)" c
    print tagset
