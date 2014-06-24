{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Robots.Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 hiding (skipSpace)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.Text as AT (isEndOfLine)
import           Data.Either           (partitionEithers)
import           Data.Char             (toUpper)
import           Data.Time.Clock
import           Data.Time.LocalTime()
import           Data.Ratio
import qualified Data.IntervalMap.FingerTree as IM

import Network.HTTP.Robots.Types


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

robotP :: Parser RobotParsing
robotP = do
  (dirs, unparsable) <- partitionEithers
                    <$> many (eitherP agentDirectiveP unparsableP) <?> "robot"
  return (dirs, filter (/= "") unparsable)

unparsableP :: Parser ByteString
unparsableP = takeTill AT.isEndOfLine <* endOfLine -- char '\n'

agentDirectiveP :: Parser ([ParsedUserAgent],[Directive])
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

agentP :: Parser ParsedUserAgent
agentP = do
  void $ stringCI "user-agent:"
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



