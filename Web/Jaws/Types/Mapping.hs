module Web.Jaws.Types.Mapping
    ( Map
    , Mapping
    , Submap
    , getSub
    , insert
    , insertSub
    , keys
    , probabilities
    , mapping
    , subToList
    , subValues
    , toList
    ) where

import qualified Data.Map          as M
import           Data.Maybe        (fromMaybe)
import           Web.Jaws.Internal
import           Web.Jaws.Text     (caps, prettyShow, wordsByLine)

type MP a     = M.Map String a
type Map      = MP Submap
type Submap   = MP Int

newtype Mapping a = Mapping (MP a)

instance Show a => Show (Mapping a) where
  show = prettyShow

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

insertSub :: Maybe Int -> String -> Submap -> Submap
insertSub Nothing  k sp = M.insert k 1 sp
insertSub (Just n) k sp = M.insert k (n + 1) sp

emptySub :: Submap
emptySub = M.singleton mempty 0

getSub :: String -> Map -> Submap
getSub k m = fromMaybe emptySub (M.lookup k m)

subValues :: Submap -> [Int]
subValues = M.elems

subToList :: Submap -> [(String, Int)]
subToList = M.toList

probabilities :: Submap -> [String]
probabilities = M.foldrWithKey go []
  where
    go k n xs = xs ++ replicate n k

keys :: Map -> [String]
keys = M.foldrWithKey consInits []
  where
    consInits k sp ks = case M.notMember "" sp of
      True  -> k : ks
      False -> ks

toList :: Maybe Submap -> [(String, Int)]
toList sp = M.toList (fromMaybe M.empty sp)
