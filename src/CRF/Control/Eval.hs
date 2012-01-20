module CRF.Control.Eval
( seqList
, seqList2
, forceList
, forceList2
) where

seqList :: [a] -> b -> b
seqList (x:xs) y = x `seq` seqList xs y
seqList [] y = y

forceList :: [a] -> [a]
forceList xs = xs `seqList` xs

seqList2 :: [[a]] -> b -> b
seqList2 (xs:xss) y = xs `seqList` seqList2 xss y
seqList2 [] y = y

forceList2 :: [[a]] -> [[a]]
forceList2 xss = xss `seqList2` xss
