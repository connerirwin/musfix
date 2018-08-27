module Language.SMT.MultiKeyMap (
  empty,
  singleton,
) where

import qualified Data.Map as Map
import Data.Map (Map)

-- | Map that allows multiple keys to reference the same value
data MultiKeyMap k a = MultiKeyMap {
  nextKey :: Int,
  keyMap :: Map k Int,
  valMap :: Map Int a
} deriving (Show)

-- | Construction
empty :: MultiKeyMap k a
empty = MultiKeyMap 0 Map.empty Map.empty

singleton :: Ord k => [k] -> a -> MultiKeyMap k a
singleton keys val = MultiKeyMap 1 kMap vMap
  where
    kMap = foldr (flip Map.insert 0) Map.empty keys
    vMap = Map.singleton 0 val

-- | Insertion
insert :: Ord k => [k] -> a -> MultiKeyMap k a -> MultiKeyMap k a
insert = insertWith const

insertWith :: Ord k => (a -> a -> a) -> [k] -> a -> MultiKeyMap k a -> MultiKeyMap k a
insertWith f keys val m = m

-- | Deletion/Update
delete :: Ord k => k -> MultiKeyMap k a -> MultiKeyMap k a
delete k m = m

adjust :: Ord k => (a -> a) -> k -> MultiKeyMap k a -> MultiKeyMap k a
adjust f k m = m

-- | Query
lookup :: Ord k => k -> MultiKeyMap k a -> Maybe a
lookup k m = do
  internalKey <- Map.lookup k $ keyMap m
  Map.lookup internalKey $ valMap m

member :: Ord k => k -> MultiKeyMap k a -> Bool
member k m = Map.member k $ keyMap m

-- | Conversion
elems :: MultiKeyMap k a -> [a]
elems m = []

keys :: MultiKeyMap k a -> [k]
keys m = []
