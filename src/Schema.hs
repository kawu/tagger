{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Schema
( Schema
, SchemedWord
, SchemedSent
, schematize
, schematize'

, Tier (..)
, mkTierDesc
, mkTiers
, select
, selectAll
) where

import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe (catMaybes)

import qualified CRF.Base as C
import qualified Data.Morphosyntax as M
import qualified Data.Morphosyntax.Tagset as M
import qualified Data.Schema as Ox

import qualified Token as T

-- | Tier section. TODO: Move to separate module?
data TierDesc = TierDesc
    { withPos   :: Bool
    , attrSet   :: S.Set M.Attr }

mkTierDesc :: L.Text -> [L.Text] -> TierDesc
mkTierDesc pos atts =
    TierDesc withPos attSet
  where
    withPos = pos `elem` atts
    attSet  = S.delete pos (S.fromList atts)

data Tier = Tier
    { hasPos    :: Bool
    , hasAttr   :: M.Attr -> Bool }

fromDesc :: TierDesc -> Tier
fromDesc TierDesc{..} = Tier
    { hasPos  = withPos
    , hasAttr = flip S.member attrSet }

mkTiers
    :: Bool         -- ^ Make complementary tier?
    -> [TierDesc]   -- ^ Attribute sets for separate tiers 
    -> [Tier]
mkTiers False tds = map fromDesc tds
mkTiers True  tds =
    map fromDesc tds ++ [comp]
  where
    atts = S.unions (map attrSet tds)
    posA = maximum (map withPos tds)
    comp = Tier (not posA) (not . flip S.member atts)

-- | TODO: This implementation assumes, that there are no identical
-- values between different attributes.  You could use tagset to solve
-- this problem, or just concatetate attribute values together with
-- their names.
select :: Tier -> M.Tag -> L.Text
select tier tag =
    L.intercalate ":" (pos ++ atts)
  where
    pos | hasPos tier = [M.pos tag]
        | otherwise   = []
    atts = catMaybes
        [ if tier `hasAttr` attr
            then Just val
            else Nothing
        | (attr, val) <- M.toList (M.atts tag) ]

selectAll :: [Tier] -> M.Tag -> [L.Text]
selectAll tiers tag = map (flip select tag) tiers

-- | Schema section.
type Schema = Ox.Schema (T.Tok M.Word)

type SchemedWord = C.Word L.Text L.Text
type SchemedSent = C.Sent L.Text L.Text

-- | On this point the input sentence is not only schematized,
-- but also all interpunction characters are removed.
-- TODO: Take tagset on account?
schematize :: [Tier] -> Schema -> M.SentMlt -> SchemedSent
schematize tiers schema cano = C.mkSent (length tiers)
    [ C.mkWord
        [ obs | _ <- tiers ]
        [ (map (flip select x) tiers, pr)
        | (x, pr) <- lbs ]
    | (obs, tok) <- zip schemed sent
    , let lbs = mergeTags (interps tok) (choice tok) ]
  where
    interps = M.interps . fst . T.body
    choice  = snd . T.body
    sent    = T.fromCano cano
    schemed = Ox.runSchema schema $ V.fromList $ map (fmap fst) sent
    mergeTags xs choice =
        let choiceMap = M.fromListWith (+)
                [ (M.tag x, pr)
                | (x, pr) <- choice ]
        in  (M.toList . M.fromList)
                [ case x `M.lookup` choiceMap of
                    Just v  -> (x, v)
                    Nothing -> (x, 0)
                | x <- map M.tag xs ]

schematize' :: [Tier] -> Schema -> M.Sent -> SchemedSent
schematize' tiers schema sent =
    schematize tiers schema [(word, []) | word <- sent]
