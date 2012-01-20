module CRF.Control.Monad.Lazy
( mapM'
-- , mapM_'
, sequence'
) where

import System.IO.Unsafe (unsafeInterleaveIO)

sequence' (mx:xs) = unsafeInterleaveIO $
    combine xs =<< mx
    where combine xs x = return . (x:) =<< sequence' xs
sequence' [] = return []

mapM' f (x:xs) = unsafeInterleaveIO $ do
    y <- f x
    ys <- mapM' f xs
    return (y : ys)
mapM' _ [] = return []

-- mapM_' f (x:xs) = unsafeInterleaveIO $ do
--     y <- f x
--     mapM_' f xs
-- mapM_' _ [] = return ()
