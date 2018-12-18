module Foo () where

import Prelude

{-@ die :: { v:_ | false } -> a @-}
die :: String -> a
die = error

{- getNth :: n:Int -> { v:[a] | 0 <= n && n < len v } -> a @-}
getNth :: Int -> [a] -> a
getNth _ []     = die "empty"
getNth 0 (x:_)  = x
getNth n (_:xs) = getNth (n - 1) xs
