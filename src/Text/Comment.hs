module Text.Comment
( removeComment
) where

removeComment :: Char -> String -> String
removeComment commChar s = case findComment s of
    Just i -> fst $ splitAt i s
    Nothing -> s
    where
        findComment s = doFind s 0 False
        doFind (x:xs) acc inQuot
            | x == commChar && not inQuot = Just acc
            | x == '"' = doFind xs (acc + 1) (not inQuot)
            | otherwise =  doFind xs (acc + 1) inQuot
        doFind [] _ _ = Nothing
