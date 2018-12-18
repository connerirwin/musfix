module Foo () where

import Prelude hiding (zip)

{-@ die :: { v:_ | false } -> a @-}
die :: String -> a
die = error

{- zip :: xs:_ -> ys:{ len ys = len xs } -> { v:_ | len v = len xs } @-}
zip :: [a] -> [b] -> [(a, b)]
zip [] []         = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys
zip _ _           = die ""
