{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Plain
( parsePlain
, parseSent
, Unknown
, Sentence
, Word
, Interp

, mkLincSent
, mkLincWord
) where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List.Split
import Data.List (groupBy, isPrefixOf)

import Text.Tagset.Data
import Text.Tagset.TagParser
import Text.Data

type Unknown = [Label]

type Sentence = [Word]
data Word = Word
    { orth  :: T.Text
    , space :: T.Text
    , forms :: [Interp] }
    deriving (Eq, Ord, Show)
data Interp = Interp
    { form  :: T.Text
    , tag   :: Label
    , dismb :: Bool }
    deriving (Eq, Ord, Show)

mkLincSent :: Sentence -> LincSent
mkLincSent = map mkLincWord

mkLincWord :: Word -> LincWord
mkLincWord Word{..} =
  LincWord orth (space == "none")
    [tag x | x <- forms]
    [prob (dismb x) | x <- forms]
  where
    prob False = 0
    prob True  = 1

parsePlain :: Tagset -> Unknown -> String -> [Sentence]
parsePlain tagset unk = map (parseSent tagset unk) . endBy "\n\n"

parseSent :: Tagset -> Unknown -> String -> Sentence
parseSent tagset unk
    = map (parseWord tagset unk . unlines)
    . groupBy (\x y -> "\t" `isPrefixOf` y)
    . lines

parseWord :: Tagset -> Unknown -> String -> Word
parseWord tagset unk inp =
    Word orth space forms
  where
    xs = lines inp
    (orth, space) = parseHeader (head xs)
    forms = nub $ concatMap (parseInterp tagset unk) (tail xs)
    nub = joinDisamb . M.toList . M.fromListWith max . cutDisamb
    joinDisamb xs =
        [ Interp form tag dismb
        | ((form, tag), dismb) <- xs ]
    cutDisamb xs =
        [ ((form, tag), dismb)
        | Interp form tag dismb <- xs ] 

parseHeader :: String -> (T.Text, T.Text)
parseHeader xs =
    let [orth, space] = splitOn "\t" xs
    in  (T.pack orth, T.pack space)

parseInterp :: Tagset -> Unknown -> String -> [Interp]
parseInterp tagset unk =
    doIt . tail . splitOn "\t"
  where
    doIt ["None", "ign"] = [Interp "None" x False | x <- unk]
    doIt [form, tag] = [Interp (T.pack form) (mkTag tag) False]
    doIt [form, tag, "disamb"] = [Interp (T.pack form) (mkTag tag) True]
    mkTag = parseTag tagset "" . T.pack
