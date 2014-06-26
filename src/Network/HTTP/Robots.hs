{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Network.HTTP.Robots where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid ((<>))
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List             (find)
import           Data.Maybe
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
        convTimeD = newDirs { timeDirectives = case timeDirectives newDirs of
                                Always     -> Always
                                JustNow im -> JustNow (mergeIntervalsWith max im)
                            }

      in Map.insert userAgents convTimeD dirsMap

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

-- Much more efficient nub . sort
nubSort :: (Eq a, Ord a) => [a] -> [a]
nubSort = Set.toAscList . Set.fromList

-- | Get all limits of a set of intervals, sorted.
getValueBy
  :: (Ord a, Ord b, Eq b, Num b)
  => (a -> a -> a)
  -> IM.IntervalMap b a
  -> b
  -> Maybe a
getValueBy f im point = l2m f . map snd . IM.search point $ im
  where l2m g l = case l of
                  [] -> Nothing
                  _  -> Just . L.foldl1' g $ l

getLargestDelay :: Ord a => IM.IntervalMap Int a -> Int -> Maybe a
getLargestDelay = getValueBy max

data SM = SMSingle | SMStart | SMEnd | SMNoth
  deriving Show

-- index by max, choose between ul and ul + 1
-- may be worth considering same value interval merging
mergeIntervalsWith
  :: (Ord a, Num b, Ord b, Eq b)
  => (a -> a -> a)
  -> IM.IntervalMap b a
  -> IM.IntervalMap b a
mergeIntervalsWith criteria' im' = let
  points = intervalMapToLimits im'
  fldl acc lst f = L.foldl' f acc lst
  in case points of
    []     -> IM.empty
    -- acc: previous value, previous starting index, new interval map
    (i:is) -> snd $ fldl (Nothing,IM.empty) (i:is) $ \(a,im) j ->
      case evalState criteria' a j (j+1) im' of
        (SMSingle,v) -> (Just j, IM.insert (IM.Interval j j) v im)
        (SMStart ,_) -> (Just j, im)
        (SMEnd   ,v) -> (Just j, IM.insert (IM.Interval (fromJust a) j) v im)
        (SMNoth  ,_) -> (     a, im)

  where
    intervalMapToLimits :: (Num b, Ord b, Eq b) => IM.IntervalMap b a -> [b]
    intervalMapToLimits iM@(IM.IntervalMap ft) = nubSort
                                            . concatMap (fromInterval . fromNode)
                                            . F.toList $ ft
      where
        fromNode (IM.Node i _) = i
        isThere im i = case IM.search i im of
          [] -> Nothing
          _  -> Just i
        fromInterval (IM.Interval x y)
          | x > 0 && y > 0 = mapMaybe (isThere iM) [x-1,x,x+1,y-1,y]
          | otherwise      = [x,y]

    evalState
      :: (Eq a, Ord a, Num b, Eq b, Ord b)
      => (a -> a -> a) -- criteria function
      -> Maybe b     -- previous index: may not exist (starting conditions)
      -> b           -- current index
      -> b           -- current index + 1
      -> IM.IntervalMap b a
      -> (SM,a)
    evalState f Nothing j z im = let
      criteria = getValueBy f
      vj = criteria im j
      vz = criteria im z
      in if vj /= vz then (SMSingle,fromJust vj) else (SMStart,fromJust vj)
    evalState f (Just a) j z im = let
      criteria = getValueBy f
      va = criteria im a
      vj = criteria im j
      vz = criteria im z
      in eval va vj vz

      where
        eval wa wj wz
          | wj /= wa && wj /= wz = (SMSingle,fromJust wj)
          | wj /= wa && wj == wz = (SMStart ,fromJust wj)
          | wj == wa && wj /= wz = (SMEnd   ,fromJust wj)
          | otherwise            = (SMNoth  ,fromJust wj)
          {-| wj == wa && wj == wz = (SMNoth  ,fromJust wj)-}


