module Web.Jaws.Types
    ( Map
    , Mapping
    , Submap
    ) where

import qualified Data.Map      as M
import           Web.Jaws.Text (prettyShow)

type MP a     = M.Map String a
type Map      = MP Submap
type Submap   = MP Int

newtype Mapping a = Mapping (MP a)

instance Show a => Show (Mapping a) where
  show = prettyShow

