{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections     #-}
module Main
    ( main
    , resourcesApp
    , Widget
    ) where

import Conduit
import RIO hiding (Data, Handler)
import RIO.Time (UTCTime (..), toGregorian, fromGregorian, getCurrentTime, defaultTimeLocale, formatTime)
import RIO.FilePath ((</>))
import RIO.Text (pack)
import qualified RIO.List as List
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Set as Set
import qualified RIO.Map as Map
import Yesod.Core
import Yesod.Static
import Yesod.Feed
import Text.Hamlet (hamletFile, shamletFile)
import Text.Lucius (luciusFile)
import Text.Julius (juliusFile)
import Data.Yaml (decodeFileThrow)
import Data.Aeson (withObject, (.:?), withText, (.!=))
import Text.Blaze (ToMarkup (..))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown
import CMarkGFM (extTable, extStrikethrough, extAutolink, optSmart, commonmarkToHtml, optUnsafe)
import qualified Data.Vector as V
import Data.Text.Read (decimal)
import System.Directory (canonicalizePath)
import System.FilePath (takeBaseName, splitExtension, splitPath)
import Data.Time (diffUTCTime)
import Shekel
import GhcInfo
import Control.AutoUpdate
import Network.HTTP.Types (renderQueryText)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder (toLazyByteString)
import System.Environment (getArgs)
import Yesod.GitRepo
import Control.Concurrent (forkIO)
import Network.HTTP.Simple (parseRequest, getResponseStatusCode, getResponseBody, httpLBS)

mkYesod "App" [parseRoutes|
/reveal/*[Text] RevealR GET
/shekel ShekelR GET
/shekel/feed ShekelFeedR GET
|]

data SocialLink = SocialLink
  { slName :: !Text
  , slIcon :: !Text
  , slPath :: !Text
  , slQuery :: ![(Text, Text)]
  }

defaultLayoutExtra :: HtmlUrl (Route App) -> Widget -> Handler Html
defaultLayoutExtra tagline widget = do
  pc <- widgetToPageContent widget
  mselfRoute <- getCurrentRoute
  withUrlRenderer $ \render -> do
    let msocialLinks = socialLinks render (pageTitle pc) <$> mselfRoute
    $(hamletFile "templates/default-layout-wrapper.hamlet") render
  where
    socialLinks render title selfRoute =
      [ SocialLink "Twitter" "twitter" "https://twitter.com/intent/tweet" [("text", titleT <> " " <> self)]
      , SocialLink "Facebook" "facebook" "https://www.facebook.com/sharer/sharer.php" [("u", self)]
      , SocialLink "LinkedIn" "linkedin" "https://www.linkedin.com/shareArticle"
          [ ("mini", "true")
          , ("url", self)
          , ("title", titleT)
          ]
      , SocialLink "Reddit" "reddit" "https://www.reddit.com/submit" [("url", self)]
      ]
      where
        self = render selfRoute []

        titleT :: Text
        titleT = TL.toStrict $ renderHtml title

    makeUrl :: Text -> [(Text, Text)] -> Text
    makeUrl path query = either impureThrow id $ decodeUtf8' $ BL.toStrict $ toLazyByteString $ encodeUtf8Builder path <> renderQueryText True (map (second Just) query)

getPostR :: Year -> Month -> Text -> Handler Html
getPostR year month slug = do
    forM_ (T.stripSuffix ".html" slug) (redirect . PostR year month)
    checkRedirects year month slug
    postsMap <- getPosts True True
    thisPost <- maybe notFound return $ Map.lookup (year, month, slug) postsMap
    posts <- getDescendingPosts False
    now <- liftIO getCurrentTime
    addPreview <- getAddPreview
    let mseriesInfo = flip fmap (postSeries thisPost) $ \(name, stitle) ->
          SeriesInfo
            { siName = name
            , siTitle = stitle
            , siPosts = flip mapMaybe (reverse posts) $ \(triple, post) -> do
                (name', _) <- postSeries post
                guard $ name == name'
                let route
                      | triple == (year, month, slug) = Nothing
                      | otherwise =
                          let (x, y, z) = triple
                           in Just $ PostR x y z
                Just (route, postTitle post, postTime post)
            }
    let tagline = [hamlet|<p class="h6 text-uppercase wt-letter-spacing-sm mb-0">Published #{prettyDay now $ postTime thisPost}|]
    defaultLayoutExtra tagline $ do
        setTitle $ toHtml $ postTitle thisPost
        forM_ (postDescription thisPost) $ \desc ->
          toWidgetHead [shamlet|<meta name=og:description value=#{desc}>|]
        forM_ (postTwitterImage thisPost) $ \url ->
          toWidgetHead [shamlet|
            <meta property=og:image content=#{url}>
            <meta name=twitter:card content=summary_large_image>
          |]
        toWidget
            [lucius|
                .post-date {
                    font-size: 20px;
                    color: #777;
                }

                #social {
                    padding: 0.5em 1em;
                    background-color: #e8e8e8;
                    border-radius: 20px;
                    margin-bottom: 10px;
                }

                #social > div {
                    display: inline-block;
                    margin-right: 1em;
                }
                ul.blog-archive {
                    padding: 0;
                }
                ul.blog-archive li {
                    list-style: none;
                    margin-bottom: 0.5em;
                }
                pre {
                    overflow: auto;
                    code {
                        word-wrap: normal;
                        white-space: pre;
                    }
                }

                #series-info {
                  margin: 0.5em;
                  padding: 0.5em;
                  background-color: #ffe;
                  border-radius: 10px;
                }
            |]
        $(whamletFile "templates/blog.hamlet")
        toWidget $(luciusFile "templates/blog.lucius")
        toWidget $(juliusFile "templates/blog.julius")
        addStylesheetRemote "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.8.0/styles/default.min.css"
        addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.8.0/highlight.min.js"
        addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.4.0/languages/haskell.min.js"
        addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.4.0/languages/rust.min.js"

getFeedR :: Handler TypedContent
getFeedR = feedHelper (const True)

getSeriesFeedR :: SeriesName -> Handler TypedContent
getSeriesFeedR name =
  feedHelper $ \post ->
    case postSeries post of
      Just (name', _) -> name == name'
      Nothing -> False

getSeriesR :: SeriesName -> Handler Html
getSeriesR name = do
  posts <- Map.filter (\p -> (fst <$> postSeries p) == Just name) <$>
           getPosts False False
  title <-
    case Map.minView posts of
      Just (p, _) | Just (_, title) <- postSeries p -> pure title
      _ -> notFound
  let makeIdent (Year y, Month m, slug) = mconcat
        [ "ident__"
        , fromString $ show y
        , "__"
        , fromString $ show m
        , "__"
        , slug
        ]
  withUrlRenderer $(hamletFile "templates/series.hamlet")

feedHelper :: (Post -> Bool) -> Handler TypedContent
feedHelper predicate = do
    posts <- (take 10 . filter (predicate . snd))
         <$> getDescendingPosts False
    updated <-
        case posts of
            [] -> notFound
            (_, post):_ -> return $ postTime post
    newsFeed Feed
        { feedTitle = "The Abominable Snoyman"
        , feedLinkSelf = FeedR
        , feedLinkHome = HomeR
        , feedAuthor = "Michael Snoyman"
        , feedDescription = "Home to strong typing, strong lifts, and more"
        , feedLanguage = "en"
        , feedUpdated = updated
        , feedLogo = Nothing
        , feedEntries = flip map posts $ \((year, month, slug), post) -> FeedEntry
            { feedEntryLink = PostR year month slug
            , feedEntryUpdated = postTime post
            , feedEntryTitle = postTitle post
            , feedEntryContent = toHtml (postContent post)
            , feedEntryEnclosure = Nothing
            }
        }

data RevealToken = RTNewColumn !Text | RTNewRow !Text
    deriving Show

getRevealDir :: Handler FilePath
getRevealDir = do
  master <- getYesod
  contentDir <- liftIO $ dataRoot <$> appData master
  liftIO $ canonicalizePath $ contentDir </> "reveal"

getRevealR :: [Text] -> Handler Html
getRevealR [] = do
  revealDir <- getRevealDir
  let stripDir x = do
        y <- List.stripPrefix revealDir x
        Just $ fromMaybe y $ List.stripPrefix "/" y
  files <- runConduit
         $ sourceDirectoryDeep False revealDir
        .| concatMapC (\fp ->
            case splitExtension fp of
              (stripDir -> Just x, ".md") ->
                Just (map pack $ splitPath x, x)
              _ -> Nothing)
        .| sinkList
  defaultLayout $ do
    setTitle "Reveal Decks"
    [whamlet|
      <div .container>
        <div .row>
          <div .col-md-2>
          <div .col-md-8>
            <h1>Reveal Decks
            <ul>
              $forall (pieces, whole) <- List.sortBy (comparing fst) files
                <li>
                  <a href=@{RevealR pieces}>#{whole}
    |]
getRevealR pieces = do
    mapM_ checkPiece pieces
    revealDir <- getRevealDir
    let fp = revealDir </> T.unpack (T.intercalate "/" pieces <> ".md")
    markdown' :: [Text] <- T.lines <$> readFileUtf8 fp `catchIO` \_ -> notFound
    let (header, drop 1 -> body) = break (== "---") $ drop 1 markdown'
        title = fromMaybe "Reveal.js Slideshow"
              $ listToMaybe
              $ mapMaybe (T.stripPrefix "title: ") header
        tokenized = tokenize RTNewColumn id body
        tokenize wrapper front [] = [wrapper $ T.unlines $ front []]
        tokenize wrapper front ("---":rest) = wrapper (T.unlines $ front []) : tokenize RTNewColumn id rest
        tokenize wrapper front ("----":rest) = wrapper (T.unlines $ front []) : tokenize RTNewRow id rest
        tokenize wrapper front (x:rest) = tokenize wrapper (front . (x:)) rest
        cols = toCols tokenized
        toCols [] = []
        toCols (RTNewColumn content:rest) =
          let (cols', rest') = getRows id rest
           in (content:cols') : toCols rest'
        toCols (RTNewRow content:rest) = toCols (RTNewColumn content:rest)

        getRows front (RTNewRow content:rest) = getRows (front . (content:)) rest
        getRows front rest = (front [], rest)
    withUrlRenderer $(hamletFile "reveal.hamlet")
  where
    checkPiece t =
      case T.uncons t of
        Nothing -> notFound
        Just ('.', _) -> notFound
        _ | T.any (== '/') t -> notFound
        _ -> return ()

data RawPosts = RawPosts ![PostRaw] !(Map SeriesName Text)

instance FromJSON RawPosts where
  parseJSON v = asObject v <|> asArray
    where
      asArray = RawPosts <$> parseJSON v <*> pure mempty
      asObject = withObject "RawPosts" $ \o -> RawPosts
        <$> o .: "posts"
        <*> o .: "series"

data PostRaw = PostRaw FilePath Text UTCTime Bool !(Maybe Text) !(Maybe Text) !(Set Text) !(Maybe SeriesName)
instance FromJSON PostRaw where
    parseJSON = withObject "PostRaw" $ \o -> PostRaw
        <$> o .: "file"
        <*> o .: "title"
        <*> ((flip UTCTime 0 <$> (o .: "day")) <|> (o .: "time"))
        <*> o .:? "listed" .!= True
        <*> o .:? "description"
        <*> o .:? "twitter-image"
        <*> o .:? "old-slugs" .!= mempty
        <*> o .:? "series"

getShekelR :: Handler Html
getShekelR = do
  currentRef <- appShekel <$> getYesod
  ecurrent <- readIORef currentRef
  case ecurrent of
    Left e -> defaultLayout $ do
      setTitle "Dollar versus Shekel—not available"
      [whamlet|
        <p>The exchange rate is not currently available.
        <pre>#{e}
      |]
    Right Current {..} -> defaultLayoutExtra (const ("Updated " <> toHtml date)) $ do
      setTitle "Dollar versus Shekel"
      toWidget [lucius|
        #rateinfo {
            border: 1px solid black;
            border-radius: 7px;
            padding: 10px;
            text-align: center;

            .rate {
                display: block;
                font-size: 200%;
            }
        }
      |]
      [whamlet|
        <div .container>
          <div .row .justify-content-center>
            <div .col-4-lg>
              <p #rateinfo>
                On #{date}, $1 (United States Dollar) buys:
                <span class="rate">#{rate} ₪
                (New Israeli Shekel).
                <span class="delta">The dollar became
                <span class=#{direction}>#{delta}

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
      |]


getShekelFeedR :: Handler ()
getShekelFeedR = do
  currentRef <- appShekel <$> getYesod
  liftIO (atomRes currentRef) >>= sendWaiResponse

getBaseR :: Handler Html
getBaseR = defaultLayout $ do
  let title = "GHC/base/Cabal library versions"
  setTitle title
  versions <- getYesod >>= liftIO . appBase
  toWidget [lucius|
    table#versions {
      font-size: 140%;
      margin: 3em auto;

      border-collapse: collapse;
        th, td {
          border: 1px solid black;
          padding: 5px;
        }
    }
  |]
  [whamlet|
    <div .container>
      <div .row>
        <div .col-md-2>
        <div .col-md-8>
          <p>This table correlates GHC versions with the versions of the base and Cabal libraries it ships with.
          <table #versions>
            <thead>
              <tr>
                <th>GHC
                <th>base
                <th>Cabal
                <th>Win32
            <tbody>
              $forall (ghc, gi) <- Map.toList versions
                <tr>
                  <td>ghc-#{ghc}
                  <td>base-#{giBase gi}
                  <td>Cabal-#{giCabal gi}
                  <td>Win32-#{giWin32 gi}
  |]

getPlanetHaskellR :: Handler TypedContent
getPlanetHaskellR = do
  --req <- parseRequest "http://planet.haskell.org/atom.xml"
  req <- parseRequest "http://planet.haskell.org/rss20.xml"
  res <- httpLBS req
  if getResponseStatusCode res == 200
    then pure $ TypedContent
              --"application/atom+xml; charset=utf-8"
              "text/xml; charset=UTF-8"
              $ toContent $ getResponseBody res
    else error "Couldn't download Atom feed"
