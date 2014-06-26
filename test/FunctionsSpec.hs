{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module FunctionsSpec where
import           Network.HTTP.Robots
import           Network.HTTP.Robots.Types
import           Network.HTTP.Robots.Parser
import           System.Directory
import           Test.Hspec

import           Control.Applicative
import           Control.Monad          (filterM, forM_)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8  as BS
import qualified Data.List as L
import           Data.Maybe
import           Data.Either
import           Data.Time.Clock
import           System.IO.Unsafe       (unsafePerformIO)
import qualified Data.IntervalMap.FingerTree as IM

import Debug.Trace


mkInt :: Int -> Int -> Int -> IM.IntervalMap Int Int
mkInt a b = IM.singleton (IM.Interval a b)

(\/) :: IM.IntervalMap Int Int -> IM.IntervalMap Int Int -> IM.IntervalMap Int Int
(\/) = IM.union

{-# ANN spec ("HLint: ignore Reduce duplication"::String) #-}
spec :: Spec
spec =
  describe "mergeIntervalsWith unit tests" $ do
    let a,b,c, x,y,z, e,f,g,h :: IM.IntervalMap Int Int
        a   = mkInt  2  6 3
        b   = mkInt 10 14 5
        c   = mkInt 18 22 7

        x   = mkInt  0  4 3
        y   = mkInt  4  8 2
        z   = mkInt  8 12 4

        e   = mkInt 0 2 3
        f   = mkInt 2 4 5
        g   = mkInt 4 6 7
        h   = mkInt 1 3 6

    it "can handle separate intervals" $ do
      let abc = a \/ b \/ c
      intervalMapToLimits abc `shouldBe` [2,6,10,14,18,22]
      show (mergeIntervalsWith max abc) `shouldBe` show abc

    it "can handle consecutive intervals" $ do
      let xyz = x \/ y \/ z
      intervalMapToLimits xyz `shouldBe` [0,4,8,12]
      show (mergeIntervalsWith max xyz) `shouldBe`
        show (mkInt 0 4 3 \/ mkInt 5 7 2 \/ mkInt 8 12 4)

    it "can handle merged intervals (1)" $ do
      let ab  = a \/ b
          xyz = x \/ y \/ z
      intervalMapToLimits (xyz \/ ab) `shouldBe` [0,2,4,6,8,10,12,14]
      show (mergeIntervalsWith max (xyz \/ ab)) `shouldBe`
        show (mkInt 0 6 3 \/ mkInt 7 7 2 \/ mkInt 8 9 4 \/ mkInt 10 14 5)

    it "can handle merged intervals (2)" $ do
      let efgh = e \/ f \/ g \/ h
      intervalMapToLimits efgh `shouldBe` [0,1,2,3,4,6]
      show (mergeIntervalsWith max efgh) `shouldBe`
        show (mkInt 0 0 3 \/ mkInt 1 3 6 \/ mkInt 4 6 7)

