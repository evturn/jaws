module Text.Jaws
    ( caps
    , prettyPrint
    , prettyShow
    , wordsByLine
    ) where

import qualified Data.Char        as C
import qualified Data.List        as L
import           Text.Show.Pretty (pPrint, ppShow)

caps :: String -> String
caps xs = (C.toUpper . head) xs : tail xs

lines :: String -> [String]
lines = L.lines

words :: String -> [String]
words = L.words

wordsByLine :: String -> [[String]]
wordsByLine = (fmap words) . lines

prettyPrint :: Show a => a -> IO ()
prettyPrint = pPrint

prettyShow :: Show a => a -> String
prettyShow = ppShow
