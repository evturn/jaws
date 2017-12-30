module Data.Jaws
    ( Map
    , Mapping
    , Submap
    , insert
    , insertSubmap
    , mapping
    ) where

import qualified Data.Map  as M
import           Text.Jaws (prettyShow, wordsByLine)

type Submap    = Mapping Int
type Map       = Mapping Submap

newtype Mapping a = Mapping (M.Map String a)

instance Show a => Show (Mapping a) where
  show = prettyShow

insertSubmap :: Maybe Int -> String -> Submap -> Submap
insertSubmap Nothing  k sp = M.insert k 1 sp
insertSubmap (Just n) k sp = M.insert k (n + 1) sp

insert :: String -> String -> Map -> Map
insert k1 k2 mp = case M.lookup k1 mp of
  Nothing -> M.insert k1 (M.singleton k2 1) mp
  Just sp -> M.insert k1 (insertSubmap (M.lookup k2 sp) k2 sp) mp

mapping :: [[String]] -> Map
mapping = (foldr go M.empty) . wordsByLine
  where
    go (x:xs:xss) mp = go (xs:xss) (insert x xs mp)
    go (x:[])     mp = insert x "" mp
    go []         mp = mp
