module Data.Jaws.Mapping where

import qualified Data.List   as L
import qualified Data.Map    as M
import           Data.Maybe  (fromMaybe)
import           Text.Prints

type MP a     = M.Map String a
type Map      = MP Submap
type Submap   = MP Int

newtype Mapping a = Mapping (MP a)

instance Show a => Show (Mapping a) where
  show = strPrints

mapping :: String -> Map
mapping = (foldr go M.empty) . wordsByLine
  where
    go (x:xs:xss) mp = go (xs:xss) (insert x xs mp)
    go (x:[])     mp = insert x "" mp
    go []         mp = mp

wordsByLine :: String -> [[String]]
wordsByLine = (fmap L.words) . L.lines

insert :: String -> String -> Map -> Map
insert k1 k2 mp = case M.lookup k1 mp of
  Nothing -> M.insert k1 (M.singleton k2 1) mp
  Just sp -> M.insert k1 (insertSubmap (M.lookup k2 sp) k2 sp) mp

insertSubmap :: Maybe Int -> String -> Submap -> Submap
insertSubmap Nothing  k sp = M.insert k 1 sp
insertSubmap (Just n) k sp = M.insert k (n + 1) sp

emptySubmap :: Submap
emptySubmap = M.singleton mempty 0

lookupSubmap :: String -> Map -> Submap
lookupSubmap k m = fromMaybe emptySubmap (M.lookup k m)

keysByFrequency :: Submap -> [String]
keysByFrequency = M.foldrWithKey go []
  where
    go k n xs = xs ++ replicate n k

keys :: Map -> [String]
keys = M.foldrWithKey extractKeys []
  where
    extractKeys k sp ks = case M.notMember "" sp of
      True  -> k : ks
      False -> ks
