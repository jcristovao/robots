{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Network.HTTP.Robots where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid ((<>))
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List             (find)
import           Data.Maybe            (catMaybes,isJust,fromMaybe, fromJust)
import           Data.Time.Clock
import           Data.Time.LocalTime()
import           Data.Ratio
import qualified Data.Foldable   as F
import qualified Data.List       as L
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.FingerTree             as FT
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
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
insertTimeDirective cd ti dirs = case timeDirectives dirs of
  Always      -> dirs { timeDirectives = case ti of
                        AnyTime  -> Always
                        JustIn i -> JustNow (IM.singleton i cd)
                       }
  JustNow im  -> dirs { timeDirectives = case ti of
                        AnyTime  -> Always
                        JustIn i -> JustNow (IM.insert i cd im)
                      }

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
        newDirs' = Directives emptyTimeDirectives (buildPathTree dirs)
        newDirs  = foldr (\x acc -> let
                              (ti,r) = extractTimeDirective x
                              in insertTimeDirective r ti acc)
                          newDirs' (filterTimeDirectives dirs)

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

filterTimeDirectives :: [Directive] -> [Directive]
filterTimeDirectives = filter (\d -> case d of CrawlDelay _ _ -> True ; _ -> False)

extractTimeDirective :: Directive -> (TimeInterval, Rational)
extractTimeDirective dir = case dir of
  CrawlDelay d ti -> (ti,d)
  _ -> error ("Unexpected time directive:" ++ show dir)

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

-- | Test if a path is allowed with the given set of directives
{-timeInterval :: IM.IntervalMap DiffTime Rational -> UTCTime -> Bool-}
{-timeAllowed tds t-}
  {-| isNull tds = True-}
  {-| otherwise  = case IM.search (utctDayTime t) tds of-}
                    {-[] -> False-}
                    {-_  -> True-}

hasTimeOfDayRestrictions :: String -> Robot -> Bool
hasTimeOfDayRestrictions agent robot =
  case timeDirectives . getDirectives agent $ robot of
    Always -> False
    _      -> True

-- | Test if a path is allowed with the given set of directives
timeAllowed :: TimeDirectives -> UTCTime -> Bool
timeAllowed Always _ = True
timeAllowed (JustNow tim) t
  | isNull tim = False
  | otherwise  = case IM.search (utctDayTime t) tim of
                    [] -> False
                    _  -> True

lookAgentDirs :: String -> Map.Map UserAgents Directives -> Maybe Directives
lookAgentDirs ag agentMap = let
  indexes =  Map.keys agentMap
  relvIx  = L.find (isJust . L.find (matchBool (BS.pack ag) . getRx)) indexes
  ix = case relvIx of
        Nothing -> L.find (isJust . L.find (matchBool "*" . getRx)) indexes
        Just ix'-> Just ix'
  in join $ fmap (`Map.lookup` agentMap) ix

getDirectives :: String -> Robot -> Directives
getDirectives agent =
  fromMaybe (error "Should at least match * (1)") . lookAgentDirs agent . directives

allowed :: String -> Robot -> FilePath -> Bool
allowed agent robot = pathAllowed (pathDirectives . getDirectives agent $ robot)

allowedNow :: UTCTime -> String -> Robot -> FilePath -> Bool
allowedNow utctime agent robot fp =
     pathAllowed (pathDirectives getDirectives' ) fp
  && timeAllowed (timeDirectives getDirectives' ) utctime
    where getDirectives' = getDirectives agent robot

{-allowedWhen :: String -> Robot -> CrawlWhen-}
{-allowedNow agent robot = let-}
  {-dirs = lookAgentDirs agent . directives $ robot-}
  {-in case dirs of-}
    {-Nothing -> error "Should at least match * (3)"-}
    {-Just dirs' -> case-}

allowedNowIO :: String -> Robot -> FilePath -> IO Bool
allowedNowIO agent robot fp = (\u -> allowedNow u agent robot fp) <$> getCurrentTime

-- IsNull for types that do not provide it,
-- neither do they provide an Eq instances.
-- See http://hackage.haskell.org/package/IsNull
-- I avoid the import since we only really need this
isNull :: (F.Foldable f) => f a -> Bool
isNull = F.foldr (\_ _ -> False) True

-- | Get all limits of a set of intervals, sorted.
intervalMapToLimits :: IM.IntervalMap Int a -> [Int]
intervalMapToLimits (IM.IntervalMap ft) = L.sort
                                        . concatMap (fromInterval . fromNode)
                                        . F.toList $ ft
  where fromNode (IM.Node i _) = i
        fromInterval (IM.Interval x y) = [x,y]

getValueBy
  :: Ord a
  => (a -> a -> a)
  -> IM.IntervalMap Int a
  -> Int
  -> Maybe a
getValueBy f im point = l2m f . map snd . IM.search point $ im
  where l2m g l = case l of
                  [] -> Nothing
                  _  -> Just . L.foldl1' g $ l

getLargestDelay :: Ord a => IM.IntervalMap Int a -> Int -> Maybe a
getLargestDelay = getValueBy max

-- Interval Different Start | Same Start | Stop (Z)
data IC = IA Int | IDSt Int | ISSt Int | IZ Int

getInt :: IC -> Int
getInt (IA   i) = i
getInt (IDSt i) = i
getInt (ISSt i) = i
getInt (IZ   i) = i

-- index by max, choose between ul and ul + 1
-- may be worth considering same value interval merging
mergeIntervalsWith
  :: Ord a
  => (a -> a -> a)
  -> IM.IntervalMap Int a
  -> IM.IntervalMap Int a
mergeIntervalsWith criteria' im' = let
  points = intervalMapToLimits im'
  criteria = getValueBy criteria'
  in case points of
    []     -> IM.empty
    (i:is) -> (\(_,_,q) -> q) $
              -- j/ul is the previous point,
              -- p is the current point
              -- w I'm not sure
              -- im is the new interval map
              foldl (\(j,w,im) p ->
                case j of
                  -- Initial case
                  IA   ul -> let v = fromJust . criteria im' $ ul + 1
                                 c = case criteria im' (p + 1) of
                                     Nothing -> IZ p
                                     Just nv -> if nv == v
                                                  then ISSt p
                                                  else IDSt p

                             in case c of
                                IZ   k -> (c,v,IM.insert (IM.Interval ul p) v im)
                                IDSt k -> (c,v,IM.insert (IM.Interval ul p) v im)
                                _      -> (c,v,im)

                  -- previous was a different start
                  IDSt ul -> let v = fromJust . criteria im' $ ul + 1
                                 c = case criteria im' (p + 1) of
                                     Nothing -> IZ p
                                     Just nv -> if nv == v
                                                  then ISSt p
                                                  else IDSt p
                             in case c of
                                IZ   k -> (c,v,IM.insert (IM.Interval ul p) v im)
                                IDSt k -> (c,v,IM.insert (IM.Interval ul p) v im)
                                _      -> (c,v,im)

                  -- previous was the same time
                  ISSt ul -> let v = fromJust . criteria im' $ ul
                                 c = case criteria im' (p + 1) of
                                     Nothing -> IZ p
                                     Just nv -> if nv == v
                                                  then ISSt ul
                                                  else IDSt p
                             in case c of
                                IZ   k -> (c,v,IM.insert (IM.Interval ul p) v im)
                                IDSt k -> (c,v,IM.insert (IM.Interval ul p) v im)
                                _      -> (c,v,im)

                  -- previous was a stopping point
                  IZ ul   -> let v = fromJust . criteria im' $ ul
                                 c = case criteria im' (p + 1) of
                                     Nothing -> IZ p
                                     Just nv -> IDSt p
                             in case c of
                                IZ   k -> (c,v,im)
                                IDSt k -> (c,v,im)
                                _      -> (c,v,im)


                    ) (IA i,fromJust . criteria im' $ i,IM.empty) is

{-mergeAdjacent :: (Eq a) => IM.IntervalMap Int a -> IM.IntervalMap Int a-}
{-mergeAdjacent im' = let-}
  {-points = intervalMapToLimits im'-}
  {-in case points of-}
    {-[]    -> IM.empty-}
    {-xs    -> foldl1' (\im x -> if  ) xs-}
