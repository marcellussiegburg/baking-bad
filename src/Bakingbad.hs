module Bakingbad (Bakery (..), Food (..), sell) where

import Data.List (delete)

data Bakery = Bakery {
    inventory :: [Food],
    funds :: Float
} deriving (Eq, Show)

data Food = Food {
    pricetag :: Float
} deriving (Eq, Show)

sell :: Bakery -> Food -> Bakery
sell bakery food = Bakery {
    inventory = delete food (inventory bakery),
    funds = funds bakery + pricetag food
}
