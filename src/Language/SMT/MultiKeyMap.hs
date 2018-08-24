module Language.SMT.MultiKeyMap (
) where

import qualified Data.Map as Map
import Data.Map (Map, (!))

-- | either do an associative list or use two maps
data MultiKeyMap k a = Map [k] a

-- | This is a map that allows multiple keys to reference the same value
insert :: Ord k => [k] -> a -> MultiKeyMap k a -> MultiKeyMap k a
insert keys val m = m
