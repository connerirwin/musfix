module Foo () where

import Prelude hiding (foldl1)

{-@ die :: { v:_ | false } -> a @-}
die :: String -> a
die = error

{- foldl1 :: (a -> a -> a) -> { v:[a] | 0 < len v } -> a @-}
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ []      = die "empty"
foldl1 f (x:xs)  = foldl f x xs
