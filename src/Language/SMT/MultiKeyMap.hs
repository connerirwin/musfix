module Language.SMT.MultiKeyMap where

import Control.Monad

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Set as Set

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
insertWith f keys val m = MultiKeyMap nextKey' keyMap' valMap'
  where
    (existingKeys, newKeys) = List.partition (flip Map.member $ keyMap m) keys
    -- | All existing keys must map to the same internal key
    (internalKey, nextKey') = case matchKeys existingKeys of
      Nothing -> (nextKey m, nextKey m + 1) -- ^ Generate a new key, need to inc counter
      Just k  -> (k, nextKey m)

    keyMap' = foldr (flip Map.insert internalKey) (keyMap m) newKeys
    valMap' = Map.insertWith f internalKey val $ valMap m

    matchKeys [] = Nothing -- ^ There were no existing keys
    matchKeys keys = if length internalKeys > 1 then error $ "All existing keys must map to the same internal key" else Just $ head internalKeys
      where
        internalKeys = Set.toList $ Set.fromList $ map ((!) $ keyMap m) keys

-- | Deletion/Update
delete :: Ord k => k -> MultiKeyMap k a -> MultiKeyMap k a
delete k m = m { keyMap = keyMap', valMap = valMap' }
  where
    keyMap' = Map.delete k $ keyMap m

    -- | Value mappings can be removed if there are no longer any keys that map to it
    internalKeys = Map.elems $ keyMap m
    valMap' = Map.filterWithKey (\k _ -> k `elem` internalKeys) $ valMap m

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
