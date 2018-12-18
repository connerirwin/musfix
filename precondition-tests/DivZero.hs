module Foo () where

import Prelude

{-@ die :: { v:_ | false } -> a @-}
die :: String -> a
die = error

{- divide :: Int -> { v:Int | v != 0 } -> Int @-}
divide :: Int -> Int -> Int
divide a 0 = die "divide by zero"
divide a b = a `div` b