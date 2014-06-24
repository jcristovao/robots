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
import Data.List.Split

import Network.HTTP.Robots.Types
import Network.HTTP.Robots.Parser

import Text.Regex.PCRE.Light

import Debug.Trace

pathTreeJoinDir :: Set.Set PathDirective -> PathsDirectives -> PathsDirectives
pathTreeJoinDir dir (T.Node (p,dirs) subs) =
                 T.Node (p,dirs `Set.union` dir) subs

-- find position in tree that best matches given path
findPathInTree
  :: (a -> a -> Bool)
  -> Z.TreePos Z.Full a
  -> [a]
  -> (Z.TreePos Z.Full a,[a])
findPathInTree _    pos []             = (pos,[])
findPathInTree test pos (p:pathPieces) =
  if test (Z.label pos) p
    then case Z.firstChild pos of
      Nothing     -> (pos,pathPieces)
      Just newpos -> findPathInTree test newpos pathPieces
    else case Z.next pos of
      Nothing -> case Z.parent pos of
                        Nothing -> (pos,p:pathPieces)
                        Just pa -> (pa ,p:pathPieces)
      Just sibpos -> findPathInTree test sibpos (p:pathPieces)

insertPathInTree
  :: Z.TreePos Z.Full (String, Set.Set PathDirective)
  -> [(String, Set.Set PathDirective)]
  -> Z.TreePos Z.Full (String, Set.Set PathDirective)
insertPathInTree pos [] = pos
insertPathInTree pos pathPieces = let
  (partialPos,remain) = findPathInTree ((==) `on` fst) pos pathPieces
  in case remain of
    [] -> Z.modifyTree (pathTreeJoinDir (snd . last $ pathPieces) ) partialPos
    _  -> insertPathInTree' partialPos remain

  where
    insertPathInTree' :: Z.TreePos Z.Full a -> [a] -> Z.TreePos Z.Full a
    insertPathInTree' =
      foldl (\newPos p -> Z.insert (T.Node p []) (Z.children newPos))

-- Zip filepaths with empty directive (Set.empty) except for the last,
-- which gets the actual intended path directive
zipWithLast :: PathDirective -> [FilePath] -> [PathDir]
zipWithLast _   [] = []
zipWithLast dir (x:[]) = [(x,Set.singleton dir)]
zipWithLast dir xs =  zip (init xs) (repeat Set.empty)
                   ++ [(last xs,Set.singleton dir)]

buildPathTree :: [Directive] -> PathsDirectives
buildPathTree dirs = let
  -- initial conditions
  initLoc  = Z.fromTree emptyPathsDirectives
  -- just path directives
  filterPathDirs = filter (\d -> case d of CrawlDelay _ _ -> False ; _ -> True)
  -- process paths
  proc dir loc = Z.root $ insertPathInTree loc
                        $ zipWithLast (extractDir dir)
                                      (FP.splitPath . extractPath $ dir)


  in Z.toTree . Z.root $ foldr proc initLoc (filterPathDirs dirs)

-- find position in tree that best matches given path
findPathInTree'
  :: (a -> a -> Bool)
  -> Z.TreePos Z.Full a
  -> [a]
  -> (Z.TreePos Z.Full a,[a])
findPathInTree' _    pos []             = (pos,[])
findPathInTree' test pos (p:pathPieces) =
  if test (Z.label pos) p
    then case Z.firstChild pos of
      Nothing     -> (pos,pathPieces)
      Just newpos -> if null pathPieces
                       then (pos,pathPieces)
                       else findPathInTree' test newpos pathPieces
    else case Z.next pos of
      Nothing -> case Z.parent pos of
                        Nothing -> (pos,p:pathPieces)
                        Just pa -> (pa ,p:pathPieces)
      Just sibpos -> findPathInTree' test sibpos (p:pathPieces)


-- I should not be doing this...
-- Match first string (with possible * matching first) with second string
-- probably buggy, tested wih unit tests
asteriskCompare :: String -> String -> Bool
asteriskCompare rstr istr = trace ("asteriskCompare:" ++ show rstr ++ "::" ++ show istr) $ let
  astLst = split' "*" rstr
  in case astLst of
    -- single asterisk, match everything
    ["","*",""] -> True
    -- no asterisk, prefix match
    (s:[]) -> s `L.isPrefixOf`  istr
    -- possible text, asterisk, and then more text
    (st:_:sngl) ->
      if null st
        -- no prervious text (starts with asterisk)
        then case sngl of
            -- next, and asterisk, ignore it
            [] -> True
            -- just one element, match the end of the string
            (_:[]) -> concat sngl `L.isSuffixOf` istr
            -- more than one element, match text for first element, and
            -- recursive call asteriskCompare for the rest?
            (x:xs) -> case split' x istr of
              (_:[])  -> False -- No match
              (_:_:yr)  -> asteriskCompare (concat xs)
                                           (concat yr)
              _ -> error ("Should not happen on asteriskCompare(1):" ++ show x ++ ":" ++ show xs)
        else case split' st istr of
          (_:[]) -> False -- No match
          ("":_:yr) -> asteriskCompare (L.dropWhile (/='*') rstr)
                                       (concat yr)
          _ -> False
    _ -> error ("Should not happen in asteriskCompare(2):" ++ show astLst)

  where
    -- | Monomorphic version to avoid strange errors
    split' :: String -> String -> [String]
    split' x = split (onSublist x)



findDirective :: PathsDirectives -> FilePath -> PathDir
findDirective pds fp
  = Z.label . fst
  . findPathInTree' (asteriskCompare `on` fst) (Z.fromTree pds)
  -- NoIndex is a dummy variable, its not used here
  . zipWithLast NoIndexD
  $ FP.splitPath fp


-- crawldelay is a rational in seconds
insertTimeDirective :: Rational -> TimeInterval -> Directives -> Directives
insertTimeDirective cd ti dirs =
  dirs { timeDirectives = IM.insert ti cd (timeDirectives dirs) }

{-type UserAgentDirectives = Map.Map UserAgents Directives-}

postProcessRobots :: RobotParsing -> RobotTxt
postProcessRobots (puds,unp) = (process puds, unp)
  where
    process :: [([UserAgent],[Directive])] -> Robot
    process puds' = Robot (foldr processUDs Map.empty puds') []
    -- process user directives
    processUDs :: ([UserAgent],[Directive]) -> Map.Map UserAgents Directives
                                            -> Map.Map UserAgents Directives
    processUDs (uas, dirs) dirsMap = let
        -- if a list of user agents has the Wildcard, then
        -- discard the remaining less specific elements.
        userAgents = if Wildcard `elem` uas
                      then Set.singleton Wildcard
                      else Set.fromList uas
        {-newDirs = foldr processDirs emptyDirectives dirs-}
        newDirs = Directives IM.empty (buildPathTree dirs)
      in Map.insert userAgents newDirs dirsMap

parseRobotsTxt :: ByteString -> Either String RobotTxt
parseRobotsTxt = fmap postProcessRobots . parseRobots

extractPath :: Directive -> FilePath
extractPath dir = BS.unpack $ case dir of
  Allow p -> p
  Disallow p -> p
  NoArchive p -> p
  NoSnippet p -> p
  NoTranslate p -> p
  NoIndex p -> p
  _ -> error "Unexpected directive (1)"

extractDir :: Directive -> PathDirective
extractDir dir = case dir of
  Allow _       -> AllowD
  Disallow _    -> DisallowD
  NoArchive _   -> NoArchiveD
  NoSnippet _   -> NoSnippetD
  NoTranslate _ -> NoTranslateD
  NoIndex _     -> NoIndexD
  _ -> error "Unexpected directive (2)"


escapeRegex' :: ByteString -> ByteString -> Char -> ByteString
escapeRegex' with regex what
  | BS.null regex = regex
  | otherwise     = let
      what' = BS.singleton what
      (before,afterwith) = BS.breakSubstring what' regex
      in analyse before afterwith what
  where
    analyse bef af what
      | BS.null af = regex
      | otherwise  = bef <> with <> BS.take 1 af <> escapeRegex' with (BS.drop 1 af) what

escapeRegex :: [Char] -> ByteString -> ByteString -> ByteString
escapeRegex charList with regex = foldl (escapeRegex' with) regex charList

escRegex :: ByteString -> ByteString
escRegex = escapeRegex "*" "."
         . escapeRegex "\\^?.+{[(|)]}" "\\"

matchBool :: Regex -> ByteString -> Bool
matchBool r bs = isJust (match r bs [])

-- I lack the art to make this prettier.
-- Currently does not take into account the canAccess :: ByteString -> RobotParsing -> Path -> Bool
canAccess :: ByteString -> RobotParsing -> Path -> Bool
canAccess _ _ "/robots.txt" = True -- special-cased
canAccess agent (robot,_) path = case stanzas of
  [] -> True
  ((_,directives):_) -> trace ("DIRECTIVES : " ++ show path ++ "\n\n") $ matchingDirective $ L.sort directives
  where stanzas = catMaybes [find (any (`isLiteralSubstring` agent) . fst) robot,
                             find (               (Wildcard `elem`) . fst) robot]


        isLiteralSubstring (Literal a) us = a `BS.isInfixOf` us
        isLiteralSubstring _ _ = False
        matchingDirective [] = True
        matchingDirective (x:xs) = trace ("matchingDirective:" ++ show x) $ case x of
          Allow robot_path -> trace ("xxxxx" ++ show x) $ let
            regex = compile (escRegex robot_path) [dollar_endonly]
            in (trace ("regex: " ++ show regex ++ "path:" ++ show path)) matchBool regex path || matchingDirective xs
          Disallow robot_path -> let
            regex = compile (escRegex robot_path) [dollar_endonly]
            in not (matchBool regex path) && matchingDirective xs

          _ -> matchingDirective xs

pathAllowed :: PathsDirectives -> FilePath -> Bool
pathAllowed pds fp = let
  (_,pd) = findDirective pds fp
  in Set.member AllowD pd && not (Set.member NoIndexD pd)

lookAgentDirs :: String -> Map.Map UserAgents Directives -> Maybe Directives
lookAgentDirs ag agentMap = let
  indexes = Map.keys agentMap
  relvIx  = L.find (Set.member (Literal (BS.pack ag))) indexes
  ix = case relvIx of
        Nothing -> L.find (Set.member Wildcard) indexes
        Just ix'-> Just ix'
  in join $ fmap (`Map.lookup` agentMap) ix



allowed :: String -> Robot -> FilePath -> Bool
allowed agent robot fp = let
  dirs  = lookAgentDirs agent . directives $ robot
  in case dirs of
    Nothing     -> error "Should at least match *"
    Just dirs'  -> pathAllowed (pathDirectives dirs') fp

