module Jaws.Data
    ( Map
    , Mapping
    , getSeeds
    , Submap
    , insert
    , insertSub
    , mapping
    ) where

import qualified Data.Map  as M
import           Jaws.Text (prettyShow, wordsByLine)

type Mapping' a = M.Map String a
type Submap     = Mapping' Int
type Map        = Mapping' Submap

newtype Mapping a = Mapping (Mapping' a)

instance Show a => Show (Mapping a) where
  show = prettyShow

insertSub :: Maybe Int -> String -> Submap -> Submap
insertSub Nothing  k sp = M.insert k 1 sp
insertSub (Just n) k sp = M.insert k (n + 1) sp

insert :: String -> String -> Map -> Map
insert k1 k2 mp = case M.lookup k1 mp of
  Nothing -> M.insert k1 (M.singleton k2 1) mp
  Just sp -> M.insert k1 (insertSub (M.lookup k2 sp) k2 sp) mp

mapping :: String -> Map
mapping = (foldr go M.empty) . wordsByLine
  where
    go (x:xs:xss) mp = go (xs:xss) (insert x xs mp)
    go (x:[])     mp = insert x "" mp
    go []         mp = mp

consInits :: String -> Submap -> [String] -> [String]
consInits  k sp ks = case hasSuccessor sp of
  True  -> k : ks
  False -> ks

getSeeds :: Map -> [String]
getSeeds = M.foldrWithKey consInits []

hasSuccessor :: Submap -> Bool
hasSuccessor = M.notMember ""
