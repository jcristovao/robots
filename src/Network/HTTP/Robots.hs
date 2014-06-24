{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PackageImports #-}
module Network.HTTP.Robots where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid ((<>))
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List             (find)
import           Data.Maybe            (catMaybes,isJust)
import           Data.Time.Clock
import           Data.Time.LocalTime()
import           Data.Ratio
import qualified Data.List       as L
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified "containers" Data.Tree        as T
import qualified Data.Tree.Zipper as Z
import qualified System.FilePath  as FP
import Data.Function
import Data.Ord
import Data.List.Split

import Network.HTTP.Robots.Types
import Network.HTTP.Robots.Parser

import Text.Regex.PCRE.Light

import Debug.Trace

-- Entities should be sorted by path length (biggest first)
-- http://blogs.bing.com/webmaster/2008/06/03/robots-exclusion-protocol-joining-together-to-provide-better-documentation/
buildPathTree :: [Directive] -> PathsDirectives
buildPathTree = map (\((r,_),d) -> (r,d))
              . L.sortBy (compare `on` (Down . snd . fst))
              . map transform
              . filterPathDirectives
  where
    transform dir = (( compile (escRegex . extractPath $ dir) [dollar_endonly]
                     , BS.length . extractPath $ dir )
                    , extractPathDirective dir)


-- First match is returned (sorted by longuest path)
findDirective :: PathsDirectives -> Path -> Maybe PathDirective
findDirective pds fp = fmap snd
                     . (find (matchBool fp . fst)) $ pds


-- crawldelay is a rational in seconds
insertTimeDirective :: Rational -> TimeInterval -> Directives -> Directives
insertTimeDirective cd ti dirs =
  dirs { timeDirectives = IM.insert ti cd (timeDirectives dirs) }

postProcessRobots :: RobotParsing -> RobotTxt
postProcessRobots (puds,unp) = (process puds, unp)
  where
    process :: [([ParsedUserAgent],[Directive])] -> Robot
    process puds' = Robot (foldr processUDs Map.empty puds') []
    -- process user directives
    processUDs :: ([ParsedUserAgent],[Directive])
               -> Map.Map UserAgents Directives
               -> Map.Map UserAgents Directives
    processUDs (uas, dirs) dirsMap = let
        -- if a list of user agents has the Wildcard, then
        -- discard the remaining less specific elements.
        userAgents = if Wildcard `elem` uas
                      then [UA . toRegex $ "*"]
                      else fmap (UA . toRegex . toBS) $ uas
        {-newDirs = foldr processDirs emptyDirectives dirs-}
        newDirs = Directives IM.empty (buildPathTree dirs)
      in Map.insert userAgents newDirs dirsMap

parseRobotsTxt :: ByteString -> Either String RobotTxt
parseRobotsTxt = fmap postProcessRobots . parseRobots

extractPath :: Directive -> Path
extractPath dir = case dir of
  Allow p -> p
  Disallow p -> p
  NoArchive p -> p
  NoSnippet p -> p
  NoTranslate p -> p
  NoIndex p -> p
  _ -> error "Unexpected directive (1)"

extractPathDirective :: Directive -> PathDirective
extractPathDirective dir = case dir of
  Allow _       -> AllowD
  Disallow _    -> DisallowD
  NoArchive _   -> NoArchiveD
  NoSnippet _   -> NoSnippetD
  NoTranslate _ -> NoTranslateD
  NoIndex _     -> NoIndexD
  _ -> error "Unexpected directive (2)"

filterPathDirectives :: [Directive] -> [Directive]
filterPathDirectives = filter (\d -> case d of CrawlDelay _ _ -> False ; _ -> True)

escapeRegex :: String -> ByteString -> ByteString -> ByteString
escapeRegex charList with' regex = foldl (escapeRegex' with') regex charList
  where
    escapeRegex' :: ByteString -> ByteString -> Char -> ByteString
    escapeRegex' with regx what
      | BS.null regx = regx
      | otherwise     = let
          what' = BS.singleton what
          (before,afterwith) = BS.breakSubstring what' regx
          in analyse before afterwith what
      where
        analyse bef af wht
          | BS.null af = regx
          | otherwise  = bef <> with <> BS.take 1 af
                      <> escapeRegex' with (BS.drop 1 af) wht

-- | Robots.txt regular expressions only feature * and $
escRegex :: ByteString -> ByteString
escRegex = escapeRegex "*" "."
         . escapeRegex "\\^?.+{[(|)]}" "\\"

toRegex :: ByteString -> Regex
toRegex rg = compile (escRegex rg) [dollar_endonly]

-- | Match a robots.txt regular expression
matchBool :: ByteString -> Regex -> Bool
matchBool bs regex = isJust (match regex bs [])

-- I lack the art to make this prettier.
-- Currently does not take into account the canAccess :: ByteString -> RobotParsing -> Path -> Bool
canAccess :: ByteString -> RobotParsing -> Path -> Bool
canAccess _ _ "/robots.txt" = True -- special-cased
canAccess agent (robot,_) path = case stanzas of
  [] -> True
  ((_,directives):_) -> matchingDirective $ L.sort directives
  where stanzas = catMaybes [find (any (`isLiteralSubstring` agent) . fst) robot,
                             find (               (Wildcard `elem`) . fst) robot]


        isLiteralSubstring (Literal a) us = a `BS.isInfixOf` us
        isLiteralSubstring _ _ = False
        matchingDirective [] = True
        matchingDirective (x:xs) = case x of
          Allow robot_path    ->      matchBool path (toRegex robot_path)
                              || matchingDirective xs
          Disallow robot_path -> not (matchBool path (toRegex robot_path))
                              && matchingDirective xs

          _ -> matchingDirective xs

-- | Test if a path is allowed with the given set of directives
pathAllowed :: PathsDirectives -> FilePath -> Bool
pathAllowed pds fp = case findDirective pds (BS.pack fp) of
    Nothing     -> True
    Just AllowD -> True
    _           -> False

lookAgentDirs :: String -> Map.Map UserAgents Directives -> Maybe Directives
lookAgentDirs ag agentMap = let
  indexes =  Map.keys agentMap
  relvIx  = L.find (isJust . L.find (matchBool (BS.pack ag) . getRx)) indexes
  ix = case relvIx of
        Nothing -> L.find (isJust . L.find (matchBool "*" . getRx)) indexes
        Just ix'-> Just ix'
  in join $ fmap (`Map.lookup` agentMap) ix

allowed :: String -> Robot -> FilePath -> Bool
allowed agent robot fp = let
  dirs  = lookAgentDirs agent . directives $ robot
  in case dirs of
    Nothing     -> error "Should at least match *"
    Just dirs'  -> pathAllowed (pathDirectives dirs') fp
