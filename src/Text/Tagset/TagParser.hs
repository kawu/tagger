module Text.Tagset.TagParser
( tagParser
, parseTag
) where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Tagset.Data
import Text.Tagset.DefParser (parseTagset)

(<<) :: Monad m => m a -> m b -> m a
(<<) x y = do {v <- x; y; return v}

sepTagParser tagset = tagParser tagset

tagParser tagset = choice
    [ ruleParser tagset rule
    | rule <- ruleDefs tagset ]

ruleParser tagset rule = do
    xs  <- try $ do
        pos <-
            let (attr, val) = head $ rulePred rule
            in valueParser attr val
        atts <- sequence
            [ valueParser attr val
            | (attr, val) <- tail $ rulePred rule ]
        return $ pos : atts
    xs' <- fmap catMaybes $ sequence
        [ withOptionParser optionMaybe optional (attrParser tagset attr)
        | (attr, optional) <- attsFor rule ]
    return $ xs ++ xs'

withOptionParser optionParser optional p =
    if not optional
        then justify p
        else optionParser p
    where justify p = do {x <- p; return $ Just x} 

attrParser tagset attr = choice
    [ valueParser attr val
    | val <- attrValues tagset attr ]
  <?> (attr ++ " value")

-- Version with separators
valueParser attr val = do
    try $ string val
    (char ':' >> return ()) <|> eof
    return (T.pack attr, T.pack val)

parseTag :: Tagset -> String -> T.Text -> Label
parseTag tagset src contents =
    case parse (tagParser tagset) src contents of
        Left e -> error $ "\nerror in tag parsing:\n"
            ++ show (T.unpack contents)
            ++ show e ++ "\n"
        Right r -> r
