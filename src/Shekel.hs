{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Shekel
  ( CurrentRef
  , withCurrentRef
  , htmlRes
  , atomRes
  ) where

import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html.Renderer.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import System.Environment (getEnv)
import Data.Text (Text)
import Text.Hamlet.XML (xml)
import Data.Text.Lazy (toStrict)
import Text.XML
import Text.XML.Cursor
import qualified Data.Map as Map
import Control.Concurrent
import Data.IORef
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Data.Time
import Text.Read (readMaybe)
import Control.Concurrent.Async (race)
import ClassyPrelude.Yesod (tryAnyDeep, SomeException, Generic, fromStrict, encodeUtf8, pack)
import Control.DeepSeq (NFData)

read' :: (Read a, Monad m) => String -> m a
read' s =
    case readMaybe s of
        Nothing -> fail $ "Could not parse: " ++ show s
        Just a -> return a

type CurrentRef = IORef (Either SomeException Current)

withCurrentRef :: (CurrentRef -> IO a) -> IO a
withCurrentRef inner = do
  currentRef <- getCurrent >>= newIORef
  either id id <$> race (populate currentRef) (inner currentRef)

getCurrent :: IO (Either SomeException Current)
getCurrent = tryAnyDeep $ do
    lbs <- simpleHttp "http://www.boi.org.il/currency.xml"
    let c = fromDocument $ parseLBS_ def lbs
    let rawDate = T.concat $ c $/ element "LAST_UPDATE" &/ content
    date' <- read' $ T.unpack rawDate :: IO Day
    let usd = head $ c $/ element "CURRENCY" &/ element "CURRENCYCODE" >=> check (\c -> T.concat (c $/ content) == "USD") >=> parent
        rate' = T.concat $ usd $/ element "RATE" &/ content
        change = T.concat $ usd $/ element "CHANGE" &/ content
    now <- getCurrentTime
    return Current
        { date = T.pack $ formatTime defaultTimeLocale "%B %e, %Y" date'
        , rate = rate'
        , delta =
            case T.uncons change of
                Just ('-', rest) -> T.concat [rest, "% weaker"]
                _ -> T.concat [change, "% stronger"]
        , updated = T.pack $ formatTime defaultTimeLocale "%FT%X-00:00" now
        , day = rawDate
        , direction =
            case T.uncons change of
                Just ('-', rest) -> "weaker"
                _ -> "stronger"
        }

populate :: CurrentRef -> IO a
populate icurrent = forever $ do
    threadDelay $ 1000 * 1000 * 60 * 60 * 3 -- 3 hours
    forkIO $ do
      eres <- getCurrent
      case eres of
        Left e -> putStrLn $ "Error loading shekel conversion: " ++ show e
        Right x -> writeIORef icurrent (Right x)

htmlRes :: CurrentRef -> IO Response
htmlRes currentRef = do
  ecurrent <- readIORef currentRef
  return $
    case ecurrent of
      Left e ->
        responseLBS status500 [("Content-Type", "text/plain")]
        $ fromStrict $ encodeUtf8 $ pack $ "Problem getting current exchange rate: " ++ show e
      Right current -> responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (homepage current)

atomRes :: CurrentRef -> IO Response
atomRes currentRef = do
  ecurrent <- readIORef currentRef
  return $
    case ecurrent of
      Left e ->
        responseLBS status500 [("Content-Type", "text/plain")]
        $ fromStrict $ encodeUtf8 $ pack $ "Problem getting current exchange rate: " ++ show e
      Right current ->
        responseLBS status200 [("Content-type", "application/atom+xml")] (feed current)

homepage Current {..} = renderHtml [shamlet|
$doctype 5
<html>
    <head>
        <meta charset="utf-8">
        <title>Dollar versus Shekel- Updated #{date}
        <link href="https://feeds.feedburner.com/DollarVersusShekel" type="application/atom+xml" rel="alternate" title="Daily dollar versus shekel updates">
        <meta name="description" content="Daily updates of the dollar versus shekel extra rate">
        <style>body{text-align:center;font-size:140%;margin:0;padding:0;font-family:sans-serif}.rate{font-size:250%;margin:10px;color:#c60}a,a:visited{color:blue;text-decoration:none}a:hover{text-decoration:underline}.footer{margin-top:20px;border-top: 1px dashed #000;padding-top:20px}.stronger{color:green}.weaker{color:red}.stronger,.weaker{font-size:130%}#content{border: 1px solid #000;width:600px;margin:0 auto;background:#eee}
        <script>var _gaq = _gaq || []; _gaq.push(['_setAccount', 'UA-1434510-21']); _gaq.push(['_trackPageview']); (function() { var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true; ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js'; var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s); })();
    <body>
        <div id="ads">
            <script>google_ad_client = "pub-4609114172551638"; google_ad_slot = "6806204452"; google_ad_width = 728; google_ad_height = 90;
            <script src="https://pagead2.googlesyndication.com/pagead/show_ads.js">
        <div id="content">
            On #{date}, $1 (United States Dollar) buys:
            <div class="rate">#{rate} ₪
            (New Israeli Shekel).
            <div class="delta">The dollar became
            <div class=#{direction}>#{delta}
        <p>You can see more currencies versus the shekel and graphs on <a href="http://www.boi.org.il/en/markets/exchangerates/pages/default.aspx">the Bank of Israel website</a>.
        <form style="border:1px solid #ccc;padding:3px;text-align:center;" action="https://blogtrottr.com" method="post" target="_blank">
            Enter your email address:
            <br>
            <input type="email" style="width:400px" name="btr_email">
            <br>
            <input type="hidden" value="https://www.snoyman.com/shekel/feed" name="btr_url">
            <input type="hidden" name="schedule_type" value="0">
            <input type="submit" value="Subscribe">
        <div>
            <a href="https://feeds.feedburner.com/DollarVersusShekel" title="Subscribe to my feed" rel="alternate" type="application/rss+xml">
                <img src="https://www.feedburner.com/fb/images/pub/feed-icon16x16.png" alt="" style="border:0"/>
            <a href="https://feeds.feedburner.com/DollarVersusShekel" title="Subscribe to my feed" rel="alternate" type="application/rss+xml">
                Subscribe in a reader
        <div>
            <div class="addthis_toolbox addthis_default_style" style="display:inline-block">
                <a href="https://www.addthis.com/bookmark.php?v=250&amp;username=snoyberg" class="addthis_button_compact">
                    Share
                <span class="addthis_separator">|
                <a class="addthis_button_preferred_1">
                <a class="addthis_button_preferred_2">
                <a class="addthis_button_preferred_3">
                <a class="addthis_button_preferred_4">
            <script type="text/javascript">var addthis_config = {"data_track_clickback":true};
            <script type="text/javascript" src="https://s7.addthis.com/js/250/addthis_widget.js#username=snoyberg">
        <div class="footer">
            This free service provided by
            <a href="https://www.snoyman.com/">Michael Snoyman
|]

data Current = Current
    { date :: Text
    , rate :: Text
    , delta :: Text
    , updated :: Text
    , day :: Text
    , direction :: Text
    }
  deriving Generic
instance NFData Current

feed Current {..} =
    renderLBS def $ Document (Prologue [] Nothing []) root []
  where
    root = Element "feed" (Map.singleton "xmlns" "http://www.w3.org/2005/Atom") nodes
    nodes = [xml|
        <title>Dollar versus Shekel
        <link rel="self" href="https://www.snoyman.com/shekel/feed">
        <link href="https://www.snoyman.com/shekel">
        <updated>#{updated}
        <id>https://www.snoyman.com/shekel
        <entry>
            <id>https://www.snoyman.com/shekel?#{day}
            <link href="https://www.snoyman.com/shekel?#{updated}">
            <updated>#{updated}
            <title>Dollar versus Shekel: #{date}
            <content type="html">#{feedHtml}
            <author>
                <name>Michael Snoyman
    |]

    feedHtml = toStrict $ Text.Blaze.Html.Renderer.Text.renderHtml [shamlet|
        On #{date}, $1 (United States Dollar) buys:<div class="rate">#{rate} ₪</div>(New Israeli Shekel).<div class="delta">The dollar became <div class=#{direction}>#{delta}</div></div>
    |]
