{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Shekel
  ( CurrentRef
  , Current (..)
  , withCurrentRef
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
import Network.HTTP.Simple (parseRequestThrow, httpBS, getResponseBody, addRequestHeader)
import qualified Data.Text as T
import Data.Time
import Text.Read (readMaybe)
import Control.Concurrent.Async (race)
import ClassyPrelude.Yesod (tryAnyDeep, SomeException, Generic, fromStrict, encodeUtf8, pack, ByteString, throwIO)
import Control.DeepSeq (NFData)
import qualified RIO.ByteString as B

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

getRaw :: IO ByteString
getRaw = do
  req <- parseRequestThrow "http://www.boi.org.il/currency.xml"
  res <- httpBS req
  let bs = getResponseBody res
  if "document.cookie" `B.isInfixOf` bs
    then do
      let cookie = B.takeWhile (/= 59) $ B.drop 1 $ B.dropWhile (/= 39) bs
      let req2 = addRequestHeader "Cookie" cookie req
      getResponseBody <$> httpBS req2
    else pure bs

getCurrent :: IO (Either SomeException Current)
getCurrent = tryAnyDeep $ do
    bs <- getRaw
    doc <- either throwIO pure $ parseLBS def $ fromStrict bs
    let c = fromDocument doc
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
