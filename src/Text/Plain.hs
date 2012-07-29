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

, applyProbs1
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

applyProbs1 :: [Double] -> Word -> Word
applyProbs1 prs word = word { forms =
    [ x { dismb = i == k }
    | (i, x) <- zip [0..] (forms word) ] }
  where
    k = fst $ maximumBy (comparing snd) (zip [0..] prs)

mkLincSent :: Sentence -> LincSent
mkLincSent = map mkLincWord

mkLincWord :: Word -> LincWord
mkLincWord Word{..} =
  LincWord (L.toStrict orth) (space == "none")
    [tag x | x <- forms]
    [prob (dismb x) | x <- forms]
  where
    -- | FIXME: ensure, that sum of probabilities == 1 (in
    -- practice, 1 or 0).
    prob False = 0
    prob True  = 1

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
    forms = nub $ concatMap (parseInterp tagset) (tail xs)
    nub = joinDisamb . M.toList . M.fromListWith max . cutDisamb
    joinDisamb xs =
        [ Interp form tag dismb
        | ((form, tag), dismb) <- xs ]
    cutDisamb xs =
        [ ((form, tag), dismb)
        | Interp form tag dismb <- xs ] 

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
