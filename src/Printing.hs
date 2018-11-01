module Printing
  ( Pattern
  , instantiatePattern
  , PrintingData(..)
  ) where

import Data.Map (Map)

----------------------------------------------------------------------------------------------------

-- | Patterns are strings, with % used to mark holes.
type Pattern = String

-- | Instantiate a pattern by putting the given strings in holes, in order.
instantiatePattern :: Pattern -> [String] -> String
instantiatePattern ('%':cs) (s:ss) = s ++ instantiatePattern cs ss
instantiatePattern (c:cs)   ss     = c : instantiatePattern cs ss
instantiatePattern []       []     = ""
instantiatePattern _        _      = error "instantiate pattern error"

-- | Describes how to turn terms into TeX.
data PrintingData = PrintingData {
    termPatterns      :: Map String Pattern,
    formulaPatterns   :: Map String Pattern,
    nounPatterns      :: Map String Pattern,
    adjectivePatterns :: Map String Pattern
  }
  deriving Show
