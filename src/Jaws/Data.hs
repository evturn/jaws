module Jaws.Data
    ( Map
    , Mapping
    , Submap
    , app
    , fromList
    , inits
    , insert
    , insertSub
    , keys
    , mapping
    , seedValues
    , toList
    ) where

import           Control.Monad.Reader
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Jaws.Internal
import           Jaws.Text            (caps, prettyShow, wordsByLine)

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

keys :: Map -> [String]
keys = M.foldrWithKey consInits []

hasSuccessor :: Submap -> Bool
hasSuccessor = M.notMember ""

toList :: Maybe Submap -> [(String, Int)]
toList sp = M.toList (fromMaybe M.empty sp)

data App s m = App
    { getSeeds :: s
    , getMap   :: m
    } deriving Show

app :: ReaderT String (App [String]) Map
app = do
  ReaderT $ \xs -> let mp = mapping xs
                    in App (keys mp) mp

-- TODO: Discontinue usage of the functions below in other modules and
--       and remove completely from library.
inits :: Map -> [String]
inits = runReaderT seedValues

fromList :: String -> Map
fromList = runReaderT mappedValues

mappedValues :: ReaderT String (M.Map String) Submap
mappedValues = ReaderT $ \r -> mapping r

seedValues :: ReaderT Map [] String
seedValues = ReaderT $ \mp -> keys mp

readerGetState :: String -> IO (String, String)
readerGetState xs = do
  mp    <- return $ fromList xs
  seeds <- return $ inits mp
  seed  <- pick seeds
  return (seed, (caps seed))

