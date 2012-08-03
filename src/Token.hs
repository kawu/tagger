{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Token
( Tok (..)
, separate
, join
, isPun
, fromCano
) where

import qualified Data.Set as S

import qualified Data.Morphosyntax.Base as M
import qualified Data.Morphosyntax.Canonical as M
import qualified Data.Morphosyntax.Tagset as M

data Tok a = Tok
    { sepB  :: [a]  -- ^ Separators before the word (perhaps null)
    , sepA  :: [a]  -- ^ Separators after the word (perhaps null)
    , body  :: a }  -- ^ The token itself
    deriving (Show, Read, Functor)

onBody :: (a -> a) -> Tok a -> Tok a
onBody f tok = tok { body = f (body tok) }

separate
    :: (a -> Bool)      -- ^ Is it a separator?
    -> [a] -> [Tok a]
separate isSep =
    reverse . uncurry setAfter . doIt [] []
  where
    doIt acc r (x:xs)
        | isSep x   = doIt (x:acc) r xs
        | otherwise = doIt [] r' xs
      where
        r'  = Tok sep [] x : r
        sep = reverse acc
    doIt acc r [] = (r, reverse acc)

    setAfter [] acc = []
    setAfter (x:xs) acc =
        x { sepA = acc } : setAfter xs (sepB x)

join :: [Tok a] -> [a]
join [] = []
join xs@(x:_) =
    let f x = body x : sepA x
    in  sepB x ++ concatMap f xs

isPun :: M.Word -> Bool
isPun w
    | xs == xs' = True
    | otherwise = False
  where
    xs  = S.fromList . map (M.pos . M.tag) . M.interps $ w
    xs' = S.singleton "interp"

fromCano :: [(M.Word, a)] -> [Tok (M.Word, a)]
fromCano = separate (isPun . fst)
