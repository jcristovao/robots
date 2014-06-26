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


  -- this is just an ugly burn-in test - we collect examples of
  -- robots.txt and check we can read them all.


{-# ANN spec ("HLint: ignore Reduce duplication"::String) #-}
spec :: Spec
spec =
  describe "mergeIntervalsWith unit tests" $ do
    let a,b,c,ab,ac,abc,x,y,z,xyz :: IM.IntervalMap Int Int
        a   = IM.singleton (IM.Interval  2  4) 3
        b   = IM.singleton (IM.Interval  6  9) 5
        c   = IM.singleton (IM.Interval 11 15) 7
        ab  = a `IM.union` b
        ac  = a `IM.union` c
        bc  = b `IM.union` c
        abc = a `IM.union` b `IM.union` c
        x   = IM.singleton (IM.Interval  1  3) 3
        y   = IM.singleton (IM.Interval  3  5) 2
        z   = IM.singleton (IM.Interval  5  7) 4
        xyz = x `IM.union` y `IM.union` z
    it "can handle separate intervals" $
      show (mergeIntervalsWith max abc) `shouldBe` show abc

    it "can handle consecutive intervals" $
      show (mergeIntervalsWith max xyz) `shouldBe` show xyz
