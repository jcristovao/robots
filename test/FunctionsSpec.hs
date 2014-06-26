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
      show (mergeIntervalsWith max abc) `shouldBe` show abc

    it "can handle consecutive intervals" $ do
      let xyz = x \/ y \/ z
      show (mergeIntervalsWith max xyz) `shouldBe` show xyz

    it "can handle merged intervals (1)" $ do
      let ab  = a \/ b
          xyz = x \/ y \/ z
      show (mergeIntervalsWith max (xyz \/ ab)) `shouldBe`
        show (mkInt 0 6 3 \/ mkInt 6 8 2 \/ mkInt 8 10 4 \/ mkInt 10 14 5)

    it "can handle merged intervals (2)" $ do
      let efgh = e \/ f \/ g \/ h
      show (mergeIntervalsWith max efgh) `shouldBe`
        show (mkInt 0 1 3 \/ mkInt 1 3 6 \/ mkInt 3 4 5 \/ mkInt 4 6 7)

