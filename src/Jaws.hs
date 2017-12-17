module Jaws where

import           Data.Char        (isAlphaNum, toLower, toUpper)
import           Data.List        (lines, nub, words)
import qualified Data.Map         as M
import           Data.Maybe       (fromMaybe)
import           System.Random
import           Text.Show.Pretty (pPrint)

type Mapping a = M.Map String a
type Map       = Mapping SubMap
type SubMap    = Mapping Int

insert :: String -> String -> Map -> Map
insert x y mp = case M.lookup x mp of
  Nothing  -> M.insert x (M.singleton y 1) mp
  Just smp -> case M.lookup y smp of
                Nothing -> M.insert x (M.insert y 1 smp) mp
                Just n  -> M.insert x (M.insert y (n + 1) smp) mp

caps :: String -> String
caps xs = (toUpper . head) xs : tail xs

getRandomInt :: (Num a, Random a) => a -> IO a
getRandomInt x = getStdRandom (randomR (0, x - 1))

parseFile :: String -> [[String]]
parseFile xs = (fmap words) $ lines xs

mapWords :: [[String]] -> Map
mapWords = foldr go M.empty
  where
    go (x:xs:xss) mp = go (xs:xss) (insert x xs mp)
    go (x:[])     mp = insert x "" mp
    go []         mp = mp

createMap :: String -> Map
createMap = (mapWords . parseFile)

getSelections :: Maybe SubMap -> [String]
getSelections smp = foldr go [] $ M.toList (fromMaybe M.empty smp)
  where
    go (k, v) xs = xs ++ replicate v k

sumElems :: Maybe SubMap -> Int
sumElems Nothing     = 0
sumElems (Just smps) = sum $ M.elems smps

getInitValues :: Map -> [String]
getInitValues mp = M.foldrWithKey go [] mp
  where
    go k smp ks = case M.notMember "" smp of
                    True  -> k : ks
                    False -> ks

getInitValue :: Map -> IO String
getInitValue mp = do
  seeds <- return $ getInitValues mp
  index <- getRandomInt (length $ seeds)
  return $ seeds !! index

getInitState :: Map -> IO (String, String)
getInitState mp = do
  seed <- getInitValue mp
  return (seed, (caps seed))

runBuilder :: (String, String) -> Map -> IO String
runBuilder state mp = do
  smp     <- return $ M.lookup (fst state) mp
  num     <- return $ sumElems smp
  choices <- return $ getSelections smp
  index   <- getRandomInt num
  word    <- return $ choices !! index
  case word of
    "" -> return $ snd state
    _  -> runBuilder (word, (snd state ++ " " ++ word)) mp

printContents :: FilePath -> IO ()
printContents p = do
  xs    <- readFile p
  mp    <- return $ createMap xs
  state <- getInitState mp
  str   <- runBuilder state mp
  pPrint str
