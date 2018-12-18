module Foo () where

import Prelude hiding (last)

{-@ die :: { v:_ | false } -> a @-}
die :: String -> a
die = error

{- last :: { v:[a] | 0 < len v } -> a @-}
last :: [a] -> a
last []     = die "empty"
last [x]    = x
last (_:xs) = last xs
