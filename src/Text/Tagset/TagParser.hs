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

tagParser tagset = choice [ tagParserFor tagset rule
                          | rule <- ruleDefs tagset ]

tagParserFor tagset rule = do
    let view = tagView tagset

        optionParser MorphosView = \p ->
            -- (p >>= return . Just) <|> (char '_' >> return Nothing)
            (p >>= return . Just) <|> return Nothing
        optionParser NKJPView = optionMaybe

    -- using 'try', since the parser should not consume
    -- any input if the rule doesn't match.
    xs  <- try $ do
        pos <- let (attr, val) = head $ rulePred rule
                in valueParser view attr val
        if view == MorphosView
            then char '.' >> return ()
            else return ()
        atts <- sequence [ valueParser view attr val
                         | (attr, val) <- tail $ rulePred rule ]
        return $ pos : atts
    xs' <- fmap catMaybes
         $ sequence [ withOptionParser (optionParser view) optional
                      $ attrParser tagset attr
                    | (attr, optional) <- attsFor rule ]
    return $ xs ++ xs'

withOptionParser optionParser optional p =
    if not optional
        then justify p
        else optionParser p
    where justify p = do {x <- p; return $ Just x} 

attrParser tagset attr =
    choice [ valueParser (tagView tagset) attr val
           | val <- attrValues tagset attr ]
           <?> (attr ++ " value")

-- Version with separators
valueParser view attr val = do
    try $ string val
    if view == NKJPView
        then (char ':' <|> space) >> return ()
        else return ()
    return (T.pack attr, T.pack val)

parseTag :: Tagset -> String -> T.Text -> IO Label
parseTag tagset src contents =
    case parse (tagParser tagset) src contents of
        Left e -> fail $ "Error parsing input:\n" ++ show e
        Right r -> return r

-- main = do
--     c <- getContents
--     tagset <- parseTagset "(stdin)" c
--     result <- parseTag tagset "V:H:N:s:mo:P:"
--     print result

