{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Plain
( parsePlain
, parseSent
, Sentence
, Word
, Interp

, showPlain
, showSent
, showWord

, mkLincSent
, mkLincWord

, applyChoice
) where

import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Data.List (groupBy, isPrefixOf, maximumBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Data.Map as M

import Text.Tagset.Data
import Text.Tagset.TagParser
import Text.Tagset.TagPrinter (showTag)
import Text.Data hiding (orth)

type Sentence = [Word]
data Word = Word
    { orth  :: L.Text
    , space :: L.Text
    , forms :: [Interp] }
    deriving (Eq, Ord, Show)
data Interp = Interp
    { form  :: L.Text
    , tag   :: Label
    , dismb :: Bool }
    deriving (Eq, Ord, Show)

applyChoice :: Word -> Label -> Word
applyChoice word label = word { forms =
    [ form { dismb = label == tag form }
    | form <- forms word ] }

mkLincSent :: Sentence -> LincSent
mkLincSent = map mkLincWord

mkLincWord :: Word -> LincWord
mkLincWord Word{..} =
  LincWord (L.toStrict orth) (space == "none")
    (map fst choice)
    (map snd choice)
  where
    choice = (norm.nub) [(tag x, weight $ dismb x) | x <- forms]
    weight True  = 1 :: Double
    weight False = 0
    nub = M.toList . M.fromListWith (+)
    norm xs =
        let z = sum (map snd xs)
        in  [(t, w / z) | (t, w) <- xs]

parsePlain :: Tagset -> L.Text -> [Sentence]
parsePlain tagset = map (parseSent tagset ). init . L.splitOn "\n\n"

parseSent :: Tagset -> L.Text -> Sentence
parseSent tagset
    = map (parseWord tagset)
    . groupBy (\_ x -> pred x)
    . L.lines
  where
    pred = ("\t" `L.isPrefixOf`)

parseWord :: Tagset -> [L.Text] -> Word
parseWord tagset xs =
    Word orth space forms
  where
    (orth, space) = parseHeader (head xs)
    forms = concatMap (parseInterp tagset) (tail xs)
--     nub = joinDisamb . M.toList . M.fromListWith max . cutDisamb
--     joinDisamb xs =
--         [ Interp form tag dismb
--         | ((form, tag), dismb) <- xs ]
--     cutDisamb xs =
--         [ ((form, tag), dismb)
--         | Interp form tag dismb <- xs ] 

parseHeader :: L.Text -> (L.Text, L.Text)
parseHeader xs =
    let [orth, space] = L.splitOn "\t" xs
    in  (orth, space)

parseInterp :: Tagset -> L.Text -> [Interp]
parseInterp tagset =
    doIt . tail . L.splitOn "\t"
  where
    -- doIt [form, "ign"] = error "parseInterp: ign tag"
    doIt [form, tag] = [Interp form (mkTag tag) False]
    doIt [form, tag, "disamb"] = [Interp form (mkTag tag) True]
    doIt xs = error $ "parseInterp: " ++ show xs
    mkTag = parseTag tagset "" . L.toStrict

-- | Printing.

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

showPlain :: Tagset -> [Sentence] -> L.Text
showPlain tagset =
    L.toLazyText . mconcat  . map (\xs -> buildSent tagset xs <> "\n")

showSent :: Tagset -> Sentence -> L.Text
showSent tagset = L.toLazyText . buildSent tagset

showWord :: Tagset -> Word -> L.Text
showWord tagset = L.toLazyText . buildWord tagset

buildSent :: Tagset -> Sentence -> L.Builder
buildSent tagset = mconcat . map (buildWord tagset)

buildWord :: Tagset -> Word -> L.Builder
buildWord tagset word
    =  L.fromLazyText (orth word) <> "\t"
    <> L.fromLazyText (space word) <> "\n"
    <> mconcat [buildInterp tagset x | x <- forms word]

buildInterp :: Tagset -> Interp -> L.Builder
buildInterp tagset interp =
    "\t" <> L.fromLazyText (form interp) <>
    "\t" <> L.fromText (showTag tagset $ tag interp) <>
    if dismb interp
        then "\tdisamb\n"
        else "\n"
