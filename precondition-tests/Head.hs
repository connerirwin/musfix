module Foo () where

import Prelude hiding (head)

{-@ die :: { v:_ | false } -> a @-}
die :: String -> a
die = error

{- head :: { xs:[a] | 0 < len xs } -> a @-}
head :: [a] -> a
head []     = die "empty"
head (x:xs) = x
