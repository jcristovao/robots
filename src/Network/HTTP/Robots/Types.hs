{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Robots.Types where

import           Data.ByteString.Char8 (ByteString)
import           Data.Time.Clock
import           Data.Time.LocalTime()
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Text.Regex.PCRE.Light

------------------------------------------------------------------------------
-- Parsing types -------------------------------------------------------------
------------------------------------------------------------------------------

type RobotParsing = ([([UserAgent], [Directive])], [Unparsable])

type Unparsable = ByteString

data UserAgent = Wildcard | Literal ByteString
  deriving (Show,Eq,Ord)

type Path = ByteString

-- Crawldelay may have a decimal point
-- http://help.yandex.com/webmaster/controlling-robot/robots-txt.xml
-- Added directives NoArchive, NoSnippet, NoTranslate, SiteMap.
-- http://bloganddiscussion.com/anythingcomputer/1/robots-txt-noarchive-nocache-nosnippet/
data Directive = Allow Path
               | Disallow Path
               | CrawlDelay { crawlDelay    :: Rational
                            , timeInterval  :: TimeInterval
                            }
               | NoArchive Path
               | NoSnippet Path
               | NoTranslate Path
               -- not used by Google, Yahoo or Live Search/Bing
               -- http://searchengineland.com/a-deeper-look-at-robotstxt-17573
               | NoIndex Path
  deriving (Show,Eq,Ord)

------------------------------------------------------------------------------
-- Processing types ----------------------------------------------------------
------------------------------------------------------------------------------

-- http://www.conman.org/people/spc/robots2.html
-- This was never actually accepted as a standard,
-- but some sites do use it.
type TimeInterval = IM.Interval DiffTime

type UserAgents = Set.Set UserAgent

data PathDirective = AllowD
                   | DisallowD
                   | NoArchiveD
                   | NoSnippetD
                   | NoTranslateD
                   | NoIndexD
    deriving (Eq,Ord,Show)

type PathDir = (Regex, PathDirective)
type PathsDirectives = [PathDir]

showPathDirective :: PathDir -> String
showPathDirective (pregex,dirs) = show pregex ++ ": " ++ show dirs

emptyPathsDirectives :: PathsDirectives
emptyPathsDirectives = []

type TimeDirectives = IM.IntervalMap TimeInterval

data Directives = Directives
  { timeDirectives :: IM.IntervalMap DiffTime Rational
  , pathDirectives :: PathsDirectives
  -- dependent on acceptance of patch for FingerTree
  } deriving Show

emptyDirectives :: Directives
emptyDirectives = Directives IM.empty emptyPathsDirectives

data Robot = Robot
  { directives :: Map.Map UserAgents Directives
  , siteMaps   :: [ByteString] -- TODO
  -- dependent on acceptance of patch for FingerTree
  }

instance Show Robot where
  show (Robot dirs _) =
    Map.foldlWithKey (\acc k v -> acc
                     ++ show k ++ "\n---------------------------\n"
                     ++ (foldl (\a x -> a ++ showPathDirective x ++ "\n") [] . pathDirectives $ v)
                     ++ "\n \n") "" dirs

type RobotTxt = (Robot,[Unparsable])


