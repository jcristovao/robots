{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Network.HTTP.Robots where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 hiding (skipSpace)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.Text as AT (isEndOfLine)
import           Data.Either           (partitionEithers)
import           Data.List             (find)
import           Data.Maybe            (catMaybes)
import           Data.Char             (toUpper)
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

import Debug.Trace

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

type PathDir = (String, Set.Set PathDirective)
type PathsDirectives = T.Tree PathDir

emptyPathsDirectives :: PathsDirectives
emptyPathsDirectives = T.Node ("/",Set.empty) []

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
  . findPathInTree' (flip asteriskCompare `on` fst) (Z.fromTree pds)
  -- NoIndex is a dummy variable, its not used here
  . zipWithLast NoIndexD
  $ FP.splitPath fp

{-comparePaths :: PathDir -> PathDir -> Bool-}
{-comparePaths (p1,_) (p2,_) = case p1 of-}
  {-[] -> case p2 of-}
    {-[] -> True-}
    {-_  -> False-}
  {-"*" -> True-}
  {-p   -> case p2 of-}
    {-"*" -> True-}
    {-p'  -> if last p' =-}

type TimeDirectives = IM.IntervalMap TimeInterval

data Directives = Directives
  { timeDirectives :: IM.IntervalMap DiffTime Rational
  , pathDirectives :: PathsDirectives
  -- dependent on acceptance of patch for FingerTree
  } deriving Show

emptyDirectives :: Directives
emptyDirectives = Directives IM.empty emptyPathsDirectives

insertPathDirective :: Path -> PathDirective -> Directives -> Directives
insertPathDirective p d dirs = undefined
  {-dirs { pathDirectives = Map.insertWith (++) p [d] (pathDirectives dirs) }-}

-- crawldelay is a rational in seconds
insertTimeDirective :: Rational -> TimeInterval -> Directives -> Directives
insertTimeDirective cd ti dirs =
  dirs { timeDirectives = IM.insert ti cd (timeDirectives dirs) }

{-type UserAgentDirectives = Map.Map UserAgents Directives-}

data Robot = Robot
  { directives :: Map.Map UserAgents Directives
  , siteMaps   :: [ByteString] -- TODO
  -- dependent on acceptance of patch for FingerTree
  }

instance Show Robot where
  show (Robot dirs _) =
    Map.foldlWithKey (\acc k v -> acc
                               ++ show k ++ "\n------\n"
                               ++ (T.drawTree . fmap show . pathDirectives $ v)
                               ++ "\n ~~~~~~~~~~~~~ \n") "" dirs

type RobotTxt = (Robot,[Unparsable])

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
    -- process directives alone
    {-processDirs :: Directive -> Directives -> Directives-}
    {-processDirs dir dirs = case dir of-}
      {-Allow p       -> insertPathDirective p AllowD       dirs-}
      {-Disallow p    -> insertPathDirective p DisallowD    dirs-}
      {-NoArchive p   -> insertPathDirective p NoArchiveD   dirs-}
      {-NoSnippet p   -> insertPathDirective p NoSnippetD   dirs-}
      {-NoTranslate p -> insertPathDirective p NoTranslateD dirs-}
      {-NoIndex p     -> insertPathDirective p NoIndexD     dirs-}
      {-CrawlDelay cd ti -> insertTimeDirective cd ti       dirs-}

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
  deriving (Show,Eq)

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


-- For use in the attoparsec monad, allows to reparse a sub expression
subParser :: Parser a -> ByteString -> Parser a
subParser p = either (const mzero) return . parseOnly p


-- Seems the rational parser is unsecure in the presence of an exponent
-- but since there is no alternative to parse a rational, we just to refuse
-- to parse numbers with 'e' / exponent
-- https://hackage.haskell.org/package/attoparsec-0.12.1.0/docs/Data-Attoparsec-ByteString-Char8.html#v:rational
safeParseRational :: Parser Rational
safeParseRational = do
  (bs,_) <- match scientific
  if BS.elem 'e' bs || BS.elem 'E' bs
    then mzero
    else subParser rational bs

-- Yeah, robots.txt should be ASCII, but some sites
-- include the UTF-8 marker at start.
-- We just drop it, but handle the file as ASCII.
dropUTF8BOM :: ByteString -> ByteString
dropUTF8BOM bs = if BS.take 3 bs == ( '\239' `BS.cons`
                                      '\187' `BS.cons`
                                      '\191' `BS.cons` BS.empty)
                   then BS.drop 3 bs
                   else bs

parseHourMinute :: Parser (Integer,Integer)
parseHourMinute = parseWithColon <|> parseWithoutColon
  where
    parseWithColon = do
      hours <- skipSpace >> decimal
      void         $ skipSpace >> char ':'
      mins  <- skipSpace >> decimal
      return (hours,mins)
    parseWithoutColon = do
      h <- Data.Attoparsec.ByteString.Char8.take 2 >>= subParser decimal
      m <- Data.Attoparsec.ByteString.Char8.take 2 >>= subParser decimal
      return (h,m)

parseTimeInterval :: Parser TimeInterval
parseTimeInterval = do
  (hours_start, mins_start) <- parseHourMinute
  void         $ (skipSpace >> char '-' >> skipSpace) <|> skipSpace
  (hours_end  , mins_end  ) <- parseHourMinute
  return $
    IM.Interval (secondsToDiffTime (hours_start * 60 * 60 + mins_start * 60))
                (secondsToDiffTime (hours_end   * 60 * 60 + mins_end   * 60))

allDay :: TimeInterval
allDay = IM.Interval (secondsToDiffTime 0)
                     (secondsToDiffTime (24*60*60)) -- because of leap seconds

parseRequestRate :: Parser Directive
parseRequestRate = do
  void $ stringCI "Request-rate:"
  docs <- skipSpace >> decimal
  void $  skipSpace >> char '/'
  ptim <- skipSpace >> decimal
  units<- skipSpace >>   (  (char 's' >> return (    1 :: Integer))
                        <|> (char 'm' >> return (   60 :: Integer))
                        <|> (char 'h' >> return (60*60 :: Integer))
                        <|>              return (    1 :: Integer)
                         )
  tint <- skipSpace >> ( parseTimeInterval <|> return allDay)
  return $ CrawlDelay ((ptim * units) % docs) tint

parseVisitTime :: Parser Directive
parseVisitTime = do
  void $ stringCI "Visit-time:"
  tint <- skipSpace >> parseTimeInterval
  return $ CrawlDelay ( 0 % 1) tint

parseCrawlDelay :: Parser Directive
parseCrawlDelay = do
  delay <- stringCI "Crawl-Delay:" >> skipSpace >> safeParseRational
  return $ CrawlDelay delay allDay

-- ... yeah.
strip :: ByteString -> ByteString
strip = BS.reverse . BS.dropWhile (==' ') . BS.reverse . BS.dropWhile (==' ')

-- | parseRobots is the main entry point for parsing a robots.txt file.
parseRobots :: ByteString -> Either String RobotParsing
parseRobots input = case parsed of
  -- special case no parsable lines and rubbish
  Right ([], out@(_:_)) ->
    Left ("no parsable lines: " ++ show out)
  _ -> parsed

  where parsed = parseOnly robotP
              . BS.unlines
  -- Filthy hack to account for the fact we don't grab sitemaps
  -- properly. people seem to just whack them anywhere, which makes it
  -- hard to write a nice parser for them.
              . filter (not . BS.isPrefixOf "SITEMAP:" . BS.map toUpper)
              . filter (not . BS.isPrefixOf "HOST:"    . BS.map toUpper)
              . filter (\x -> BS.head x /= '#' )
              . filter (not . BS.null)
              . map strip
              . BS.lines
              -- worst way of handling windows newlines ever
              . BS.filter (/= '\r')
              . dropUTF8BOM
              $ input

robotP :: Parser RobotParsing
robotP = do
  (dirs, unparsable) <- partitionEithers <$> many (eitherP agentDirectiveP unparsableP) <?> "robot"
  return (dirs, filter (/= "") unparsable)

unparsableP :: Parser ByteString
unparsableP = takeTill AT.isEndOfLine <* endOfLine -- char '\n'

agentDirectiveP :: Parser ([UserAgent],[Directive])
agentDirectiveP = (,) <$> many1 agentP <*> many1 directiveP <?> "agentDirective"


skipSpace :: Parser ()
skipSpace = skipWhile (\x -> x==' ' || x == '\t')

directiveP :: Parser Directive
directiveP = choice [ stringCI "Disallow:" >> skipSpace >>
                        ((Disallow <$> tokenP) <|>
                      -- This requires some explanation.
                      -- The RFC suggests that an empty Disallow line means
                      -- anything is allowed. Being semantically equivalent to
                      -- 'Allow: "/"', I have chosen to change it here
                      -- rather than carry the bogus distinction around.
                             (endOfLine >> return (Allow    "/")))
                    , stringCI "Allow:" >> skipSpace >>
                        ((Allow <$> tokenP) <|>
                      -- If an empty disallow means 'disallow nothing',
                      -- an empty allow means 'allow nothing'. Right?
                      -- Not sure, actually, but only the americanexpress.com
                      -- has such a case, which in one hand I am tempted
                      -- to consider an error... but for now:
                             (endOfLine >> return (Disallow "/")))
                    , parseCrawlDelay
                    , parseRequestRate
                    , parseVisitTime
                    , NoArchive   <$> (stringCI "Noarchive:"  >> skipSpace >> tokenP)
                    , NoSnippet   <$> (stringCI "Nosnippet:"  >> skipSpace >> tokenP)
                    , NoTranslate <$> (stringCI "Notranslate:">> skipSpace >> tokenP)
                    , NoIndex     <$> (stringCI "Noindex:"    >> skipSpace >> tokenP)
                    ] <* commentsP <?> "directive"

agentP :: Parser UserAgent
agentP = do
  stringCI "user-agent:"
  skipSpace
  ((string "*" >> return Wildcard) <|>
   (Literal  <$> tokenWithSpacesP)) <* skipSpace <* endOfLine <?> "agent"


commentsP :: Parser ()
commentsP = skipSpace >>
            (   (string "#" >> takeTill AT.isEndOfLine >> endOfLine)
            <|> endOfLine
            <|> return ())


tokenP :: Parser ByteString
tokenP = skipSpace >> takeWhile1 (not . isSpace) <* skipSpace
tokenWithSpacesP :: Parser ByteString
tokenWithSpacesP = skipSpace >> takeWhile1 (not . (\c -> c == '#' || AT.isEndOfLine c))
							 <* takeTill AT.isEndOfLine


-- I lack the art to make this prettier.
-- Currently does not take into account the canAccess :: ByteString -> RobotParsing -> Path -> Bool
canAccess :: ByteString -> RobotParsing -> Path -> Bool
canAccess _ _ "/robots.txt" = True -- special-cased
canAccess agent (robot,_) path = case stanzas of
  [] -> True
  ((_,directives):_) -> matchingDirective directives
  where stanzas = catMaybes [find (any (`isLiteralSubstring` agent) . fst) robot,
                             find (               (Wildcard `elem`) . fst) robot]


        isLiteralSubstring (Literal a) us = a `BS.isInfixOf` us
        isLiteralSubstring _ _ = False
        matchingDirective [] = True
        matchingDirective (x:xs) = case x of
          Allow robot_path ->
            robot_path `BS.isPrefixOf` path || matchingDirective xs
          Disallow robot_path ->
            not (robot_path `BS.isPrefixOf` path) && matchingDirective xs

          _ -> matchingDirective xs

canAccess' :: String -> RobotTxt -> Path -> Bool
canAccess' _ _ "/robots.txt" = True  -- common sense
canAccess' agent (Robot dirs _, _) p = let
  keys  = foldr Set.intersection (Set.fromList [Wildcard, Literal (BS.pack agent)])
        $ Map.keys dirs -- keys are sets
  -- now fold through the keys, and get the rules
  {-uas   = Map.findWithDefault-}
                      {-(error "Assertion failed: User agent key must be valid")-}
                      {-ua-}
                      {-dirs-}

  in undefined
  {-foldr (\ua acc -> filter (\x -> x `BS.isPrefixOf` p)-}
                  {-. Map. keys pathDirectives-}
                  {-.   in undefined-}
