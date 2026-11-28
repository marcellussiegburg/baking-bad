module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe)

import Bakingbad (Bakery (..), Food (..), sell)

main :: IO ()
main = hspec $ do
    describe "foods" $ do
        it "gets sold for funds" $ do
            let bakery = Bakery {inventory = [cake], funds = 100.00}
                cake = Food {pricetag = 10.00}
            bakery `sell` cake
                `shouldBe` Bakery {inventory = [], funds = 110.00}
