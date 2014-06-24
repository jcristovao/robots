{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module RobotSpec where
import           Network.HTTP.Robots
import           Network.HTTP.Robots.Types
import           Network.HTTP.Robots.Parser
import           System.Directory
import           Test.Hspec
import           Test.Hspec.Formatters

import           Control.Applicative
import           Control.Monad          (filterM, forM_)
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8  as BS
import           Data.Either
import           System.IO.Unsafe       (unsafePerformIO)

import           Debug.Trace

-- apparently not a utility function.
myIsLeft :: Either a b -> Bool
myIsLeft (Left _) = True
myIsLeft _        = False

dirname :: String
dirname = reverse $ dropWhile (/= '/') $ reverse __FILE__
-- forgive me father, i know not what i do.

texts :: [(FilePath, BS.ByteString)]
texts = unsafePerformIO $ do
  contents <- map ((dirname ++ "/examples/") ++)
          <$> getDirectoryContents (dirname ++ "/examples")
  files <- filterM doesFileExist contents
  mapM (\x -> BS.readFile x >>= \c -> return (x,c  )) files

frozen :: [(FilePath, BS.ByteString)]
frozen = unsafePerformIO $ do
  contents <- map ((dirname ++ "/frozen_examples/") ++)
          <$> getDirectoryContents (dirname ++ "/frozen_examples")
  files <- filterM doesFileExist contents
  mapM (\x -> BS.readFile x >>= \c -> return (x,c  )) files


  -- this is just an ugly burn-in test - we collect examples of
  -- robots.txt and check we can read them all.


{-# ANN spec ("HLint: ignore Reduce duplication"::String) #-}
spec :: Spec
spec = do
  describe "simple parsing" $ do
    it "can read a token" $
      parseOnly tokenP "foo" `shouldBe`
        Right "foo"
    it "can read a user agent" $
      parseOnly agentP "User-agent: *\n" `shouldBe`
        Right Wildcard
    it "can read a specific user agent" $
      parseOnly agentP "User-agent: Buzzbot\n" `shouldBe`
        Right (Literal "Buzzbot")
    it "can read allow directives" $
      parseOnly directiveP "Allow: /\n" `shouldBe`
        Right (Allow "/")
    it "should read a full robots.txt" $
      parseOnly robotP "User-agent: *\nDisallow: /\n"
        `shouldBe` Right ([([Wildcard], [Disallow "/"])],[])
    it "should cope with end-of-line comments" $
      parseOnly robotP "User-agent: *\nDisallow: / # don't read my site\nAllow: /foo"
        `shouldBe` Right ([([Wildcard], [Disallow "/", Allow "/foo"])],[])
    it "can parse this stupid empty disallow line that the BNF suggests should be illegal" $
      parseOnly robotP "User-agent: *\nDisallow:\n"
        `shouldBe` Right ([([Wildcard], [Allow "/"])],[])
    it "ignores the sitemap extension (and any other unrecognised text" $
      parseOnly robotP "Sitemap: http:www.ebay.com/lst/PDP_US_main_index.xml\nUser-agent: *\nDisallow: /\n"
        `shouldBe` Right ([([Wildcard], [Disallow "/"])], ["Sitemap: http:www.ebay.com/lst/PDP_US_main_index.xml"])


  describe "smoke test - check we can read all the robots.txt examples" $
    -- we should also verify if there were unparsed items
    forM_ texts $ \(name,text) ->
      it ("should parse " ++ name) $
        parseRobots text `shouldSatisfy`
          (\x -> 1 == length (rights [x])
              -- head is safe here if first condition is met
              && 0 == length (snd . head . rights $ [x]))

  describe "test frozen amazon.de" $ do
    -- Ugly hack for now
    let (name,bs) = frozen !! 0
        parsed = either (fail "should be able to parse amazon.de") id $ parseRobots bs
        robots = either (fail "should be able to parse amazon.de") fst $ parseRobotsTxt bs

    it "canAccess: should block /gp/cart" $
      canAccess "*"  parsed "/gp/cart" `shouldBe` False
    it "allowed  : should block /gp/cart" $
      allowed "*"  robots "/gp/cart" `shouldBe` False

    it "canAccess: should allow /wishlist/universal-thruth" $
      canAccess "*"  parsed "/wishlist/universal-thruth" `shouldBe` True
    it "allowed  : should allow /wishlist/universal-thruth" $
      allowed "*"  robots "/wishlist/universal-thruth" `shouldBe` True

    it "canAccess: should block /wishlist/uu" $
      canAccess "*"  parsed "/wishlist/uu" `shouldBe` False
    it "allowed  : should allow /wishlist/uu" $
      allowed "*"  robots "/wishlist/uu" `shouldBe` False


    it "canAccess: should block /gp/registry/wishlist/*/reserve" $
      canAccess "*" parsed "/gp/registry/wishlist/abc/reserve" `shouldBe` False
    it "allowed  : should block /gp/registry/wishlist/*/reserve" $
      allowed "*" robots "/gp/registry/wishlist/abc/reserve" `shouldBe` False

    it "canAccess: should block */b?ie=UTF8&node=1619732031" $
      canAccess "*" parsed "/reserve/b?ie=UTF8&node=1619732031" `shouldBe` False
    it "allowed  : should block */b?ie=UTF8&node=1619732031" $
      allowed "*" robots "/reserve/b?ie=UTF8&node=1619732031" `shouldBe` False




  -- the behaviour here doesn't seem to be rigorously specified: it
  -- seems obvious that if * can access a resource but FooBot is
  -- explicitly disallowed, then FooBot should be disallowed.

  -- so we should check first the most specific bot, then go in order
  -- of Allow/Disallow statements. Anyone playing silly buggers with
  --
  -- UserAgent: *
  -- Allow: /
  -- Disallow: /
  --
  -- is fit for treasons, stratagems and spoils.
  -- ganked from http://www.robotstxt.org/norobots-rfc.txt
  {-describe "canAccess" $ do-}
    {-let robot =-}
          {-([([Literal "unhipbot"],-}
             {-[Disallow "/"]),-}
            {-([Literal "webcrawler", Literal "excite"],-}
             {-[Allow "/"]),-}
            {-([Wildcard],-}
             {-[Disallow "/org/plans.html",-}
              {-Allow "/org/",-}
              {-Allow "/serv",-}
              {-Allow "/~mak",-}
              {-Disallow "/"])], [])-}



  --    unhipbot webcrawler other
  --             & excite
    {-let tests = [("/",-}
                  {-(False, True, False)),-}
                 {-("/index.html",-}
                  {-(False, True, False)),-}
                 {-("/robots.txt",-}
                  {-(True,      True,       True)),-}
                 {-("/server.html",-}
                  {-(False,       True,       True)),-}
                 {-("/services/fast.html",-}
                  {-(False,       True,       True)),-}
                 {-("/services/slow.html",-}
                  {-(False,       True,       True)),-}
                 {-("/orgo.gif",-}
                  {-(False,       True,       False)),-}
                 {-("/org/about.html",-}
                  {-(False,       True,       True)),-}
                 {-("/org/plans.html",-}
                  {-(False,       True,       False)),-}
                 {-("/~jim/jim.html",-}
                  {-(False,       True,       False)),-}
                 {-("/~mak/mak.html",-}
                  {-(False,       True,       True))]-}
    {-forM_ tests $ \(path, (unhip, web_and_excite, other)) -> do-}
      {-it ("unhipbot access to " ++ show path) $-}
        {-canAccess "unhipbot" robot path `shouldBe` unhip-}
      {-it ("webcrawler & excite access to " ++ show path) $ do-}
        {-canAccess "webcrawler" robot path `shouldBe` web_and_excite-}
        {-canAccess "excite" robot path `shouldBe` web_and_excite-}
      {-it ("otherbot access to " ++ show path) $-}
        {-canAccess "otherbot" robot path `shouldBe` other-}

  -- describe "allowable" $ do
  --   let robot = [([Wildcard], [Disallow "/", Allow "/anyone"]),
  --                ([Literal "OtherSpecial", Literal "SpecialBot"], [Allow "/", Allow "/only_special"])]
  --   it "should allow access to anyone" $ do
  --     canAccess "anyone"  robot "/anyone" `shouldBe` True
  --   it "should deny access to root for most bots" $ do
  --     canAccess "anyone"  robot "/" `shouldBe` False
  --   it "should deny access to only_special for most bots" $ do
  --     canAccess "anyone"  robot "/only_special" `shouldBe` False
  --   it "allows access to specialbot" $ do
  --     canAccess "SpecialBot"  robot "/" `shouldBe` True
  --   it "allows access to specialbot special area" $ do
  --     canAccess "SpecialBot"  robot "/only_special" `shouldBe` True
  --   it "allows access to specialbot special area" $ do
  --     canAccess "OtherSpecial"  robot "/only_special" `shouldBe` True

  {-describe "regressions" $-}
    {-it "chooses the most specific user agent from helloworldweb" $ do-}
      {-(Right hellobot) <- parseOnly robotP <$> liftIO (BS.readFile "./test/examples/helloworldweb2.com")-}
      {-canAccess "Mozilla/5.0 (compatible; meanpathbot/1.0; +http://www.meanpath.com/meanpathbot.html)" hellobot "/" `shouldBe` False-}
      {-canAccess "googlebot" hellobot "/" `shouldBe` True-}



  {-describe "incorrect robots files" $ do-}
    {-it "treats HTML as garbage" $-}
      {-parseRobots "<html><head>a thing</head><body>yo, i'm not a robots file</body></html>\n"-}
        {-`shouldSatisfy` myIsLeft-}


    {-it "can handle no-newline files" $-}
      {-parseRobots "<html><head>a thing</head><body>yo, i'm not a robots file</body></html>"-}
        {-`shouldSatisfy` myIsLeft-}
