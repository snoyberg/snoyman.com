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

data App = App
    { appData   :: IO Data
    , appImg    :: Static
    , appStatic :: Static
    , appTorah  :: Static
    , appWellKnown :: Static
    , appMaxi   :: Static
    , appShekel :: !CurrentRef
    , appBase :: !(IO (Map GhcVersion GhcInfo))
    }

data Data = Data
    { dataHome :: Home
    , dataRoot :: FilePath
    , dataRobots :: TypedContent
    , dataFavicon :: TypedContent
    , dataPostsAll :: Map (Year, Month, Text) (Either Post Text)
    }

dataPostsPast :: MonadIO m
              => Data
              -> m (Map (Year, Month, Text) Post)
dataPostsPast d = do
    now <- liftIO getCurrentTime
    let pred' (Left p)
          | postTime p <= now = Just p
        pred' _ = Nothing
    return $ Map.mapMaybe pred' (dataPostsAll d)

data Home = Home
    { homeTitle :: Text
    , homeToplinks :: Vector Link
    , homeAbout :: Html
    , homePubs :: Vector Pub
    , homeTalks :: Vector Talk
    , homeSites :: Vector Link
    }

renderMarkdownOld :: Text -> Html
renderMarkdownOld = markdown def
    { msXssProtect = False
    , msAddHeadingId = True
    } . TL.fromStrict

renderMarkdownNew :: Text -> Html
renderMarkdownNew =
  preEscapedToMarkup .
  commonmarkToHtml
  [optSmart, optUnsafe]
  [extAutolink, extStrikethrough, extTable]

instance FromJSON Home where
    parseJSON = withObject "Home" $ \o -> Home
        <$> o .: "title"
        <*> o .: "toplinks"
        <*> (renderMarkdownNew <$> (o .: "about"))
        <*> o .: "publications"
        <*> o .: "talks"
        <*> o .: "sites"

data Link = Link
    { _linkURL :: Text
    , _linkText :: Text
    }
instance FromJSON Link where
    parseJSON = withObject "Link" $ \o -> Link <$> o .: "url" <*> o .: "text"
instance ToMarkup Link where
    toMarkup (Link u t) = [shamlet|<a href=#{u}>#{t}|]

data Pub = Pub
    { _pubURL :: Text
    , _pubText :: Text
    , _pubDate :: Date
    , _pubPublisher :: Maybe Text
    }
instance FromJSON Pub where
    parseJSON = withObject "Pub" $ \o -> Pub
        <$> o .: "url"
        <*> o .: "text"
        <*> o .: "date"
        <*> o .:? "publisher"

data Date = YearMonth Int Int
instance FromJSON Date where
    parseJSON = withText "Date" $ \t -> parseYearMonth t
      where
        parseYearMonth t = do
            (year, t1) <- either fail pure $ decimal t
            t2 <- maybe (fail "no dash") pure $ T.stripPrefix "-" t1
            (month, "") <- either fail pure $ decimal t2
            when (month < 1 || month > 12) $ fail "Invalid month"
            pure $ YearMonth year month
instance ToMarkup Date where
    toMarkup (YearMonth year month') =
        toMarkup monthT <> " " <> toMarkup year
      where
        months :: Vector Text
        months = V.fromList $ T.words
            "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"
        monthI
            | month' < 1 = 0
            | month' > 12 = 11
            | otherwise = month' - 1
        monthT = months V.! monthI

data Talk = Talk
    { talkDate :: Date
    , talkTitle :: Text
    , talkVenue :: Text
    , talkLinks :: Either Text (Vector Link)
    }
instance FromJSON Talk where
    parseJSON = withObject "Talk" $ \o -> Talk
        <$> o .: "date"
        <*> o .: "title"
        <*> o .: "venue"
        <*> ((Left <$> (o .: "links")) <|> (Right <$> (o .: "links")))

newtype Year = Year Int
    deriving (PathPiece, Show, Eq, Read, Hashable, Ord)
newtype Month = Month Int
    deriving (Show, Eq, Read, Hashable, Ord)
instance PathPiece Month where
    toPathPiece (Month i)
        | i < 10 = "0" <> tshow i
        | otherwise = tshow i
    fromPathPiece t
        | T.length t /= 2 = Nothing
        | Right (i, "") <- decimal t, i >= 1, i <= 12 = Just $ Month i
        | otherwise = Nothing

data Post = Post
    { postTitle :: Text
    , postContent :: Html
    , postTime :: UTCTime
    , postListed :: Bool
    , postFilename :: !FilePath
    , postDescription :: !(Maybe Text)
    , postSeries :: !(Maybe (SeriesName, Text))
    }

type SeriesName = Text

mkYesod "App" [parseRoutes|
/ HomeR GET
/img ImgR Static appImg
/static StaticR Static appStatic
/torah TorahR Static appTorah
/robots.txt RobotsR GET
/favicon.ico FaviconR GET
/blog BlogR GET
/blog/#Year/#Month/#Text PostR GET
/feed FeedR GET
/feed/#SeriesName SeriesFeedR GET
/series/#SeriesName SeriesR GET
/.well-known WellKnownR Static appWellKnown
/maxi MaxiR Static appMaxi
/reveal/*[Text] RevealR GET
/shekel ShekelR GET
/shekel/feed ShekelFeedR GET
/base BaseR GET
|]

data SocialLink = SocialLink
  { slName :: !Text
  , slIcon :: !Text
  , slPath :: !Text
  , slQuery :: ![(Text, Text)]
  }

stylesheets :: Html
stylesheets = $(shamletFile "templates/stylesheets.hamlet")

topnav :: HtmlUrl (Route App)
topnav = $(hamletFile "templates/topnav.hamlet")

bodyend :: HtmlUrl (Route App)
bodyend = $(hamletFile "templates/bodyend.hamlet")

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

instance Yesod App where
    approot = guessApproot
    makeSessionBackend _ = return Nothing

    defaultLayout = defaultLayoutExtra mempty

    defaultMessageWidget title body =
        [whamlet|
            <div .container>
              <div .row>
                <div .col-md-2>
                <div .col-md-8>
                  <h1>#{title}
                  ^{body'}
        |]
      where
        body' :: Widget
        body' = toWidget body

getData :: Handler Data
getData = getYesod >>= liftIO . appData

getPosts :: Bool -- ^ include unlisted?
         -> Bool -- ^ include future posts regardless of ?preview=true?
         -> Handler (Map (Year, Month, Text) Post)
getPosts unlisted includeFuture = do
    mpreview <- lookupGetParam "preview"
    dat <- getData
    posts <-
        if mpreview == Just "true" || includeFuture
            then return
                    $ Map.mapMaybe (either Just (const Nothing))
                    $ dataPostsAll dat
            else dataPostsPast dat
    return $
        if unlisted
            then posts
            else Map.filter postListed posts

getDescendingPosts :: Bool -- ^ include unlisted?
                   -> Handler [((Year, Month, Text), Post)]
getDescendingPosts unlisted =
    reverse . List.sortOn (postTime . snd) . Map.toList <$> getPosts unlisted False

getHomeR :: Handler Html
getHomeR = do
    dat <- getData
    posts <- getDescendingPosts False
    now <- liftIO getCurrentTime
    let Home {..} = dataHome dat
        mRecentPost =
            case posts of
                ((year, month, slug), post):_ -> Just
                    ( PostR year month slug
                    , postTitle post
                    , prettyDay now $ postTime post
                    )
                [] -> Nothing
    withUrlRenderer $(hamletFile "templates/home.hamlet")

prettyDay :: UTCTime -- ^ today, for determining april fools age
          -> UTCTime -- ^ day to show
          -> String
prettyDay today x
  | (year, 4, 1) <- toGregorian (utctDay x)
  , diffUTCTime today x >= (60 * 60 * 24 * 4) = "April Fools', " ++ show year
  | (2020, 4, 1) <- toGregorian (utctDay x) = "Spring equinox 2020 + 12 days"
  | (2019, 4, 1) <- toGregorian (utctDay x) = "KAL. APR. MMDCCLXXII AUC"
  | (2018, 4, 1) <- toGregorian (utctDay x) = "Stardate 47634.4"
  | (2017, 4, 1) <- toGregorian (utctDay x) = "5 Nissan, 5777"
  | otherwise = formatTime defaultTimeLocale "%B %e, %Y" x

getFaviconR :: Handler TypedContent
getFaviconR = dataFavicon <$> getData

getRobotsR :: Handler TypedContent
getRobotsR = dataRobots <$> getData

getAddPreview :: Handler (Route App -> (Route App, [(Text, Text)]))
getAddPreview = do
    let key = "preview" :: Text
    previews <- lookupGetParams key
    return (, map (key,) previews)

getBlogR :: Handler ()
getBlogR = do
    posts <- getDescendingPosts False
    addPreview <- getAddPreview
    case posts of
        [] -> notFound
        ((year, month, slug), _):_ -> redirect $ addPreview $ PostR year month slug

checkRedirects :: Year -> Month -> Text -> Handler ()
checkRedirects year month slug = do
  dat <- getData
  case Map.lookup (year, month, slug) $ dataPostsAll dat of
    Just (Right slug') -> redirect $ PostR year month slug'
    _ -> return ()

data SeriesInfo = SeriesInfo
  { siName :: !SeriesName
  , siTitle :: !Text
  , siPosts :: ![(Maybe (Route App), Text, UTCTime)]
  }

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
        { feedTitle = "Michael Snoyman's blog"
        , feedLinkSelf = FeedR
        , feedLinkHome = HomeR
        , feedAuthor = "Michael Snoyman"
        , feedDescription = "Michael's thoughts on everything, mostly Haskell and tech startups"
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

loadData :: FilePath -> IO Data
loadData dataRoot = do
    let readData x y = do
            bs <- readFileBinary $ dataRoot </> y
            return $ TypedContent x $ toContent bs
    dataFavicon <- readData "image/x-icon" "favicon.ico"
    dataRobots <- readData "text/plain" "robots.txt"
    dataHome <- decodeFileThrow (dataRoot </> "home.yaml")

    RawPosts rawPosts allSeries <-
      decodeFileThrow (dataRoot </> "posts.yaml")
    dataPostsAll <- (Map.fromList . concat) <$> mapM (loadPost dataRoot allSeries) (rawPosts :: [PostRaw])

    return Data {..}

data RawPosts = RawPosts ![PostRaw] !(Map SeriesName Text)

instance FromJSON RawPosts where
  parseJSON v = asObject v <|> asArray
    where
      asArray = RawPosts <$> parseJSON v <*> pure mempty
      asObject = withObject "RawPosts" $ \o -> RawPosts
        <$> o .: "posts"
        <*> o .: "series"

data PostRaw = PostRaw FilePath Text UTCTime Bool !(Maybe Text) !(Set Text) !(Maybe SeriesName)
instance FromJSON PostRaw where
    parseJSON = withObject "PostRaw" $ \o -> PostRaw
        <$> o .: "file"
        <*> o .: "title"
        <*> ((flip UTCTime 0 <$> (o .: "day")) <|> (o .: "time"))
        <*> o .:? "listed" .!= True
        <*> o .:? "description"
        <*> o .:? "old-slugs" .!= mempty
        <*> o .:? "series"

loadPost
  :: FilePath
  -> Map SeriesName Text
  -> PostRaw
  -> IO [((Year, Month, Text), Either Post Text)]
loadPost dataRoot allSeries (PostRaw postFilename postTitle postTime postListed postDescription oldSlugs mseries) = do
    postSeries <- for mseries $ \name ->
      case Map.lookup name allSeries of
        Nothing -> error $ "Undefined series name: " ++ show name
        Just title -> pure (name, title)
    let renderMarkdown
          | utctDay postTime >= fromGregorian 2018 7 9 = renderMarkdownNew
          | otherwise = renderMarkdownOld
    postContent <- fmap renderMarkdown $ readFileUtf8 $ dataRoot </> postFilename
    let year' = Year $ fromIntegral year
        month' = Month month
    return
      $ ((year', month', slug), Left Post {..})
      : map (\slug' -> ((year', month', slug'), Right slug)) (Set.toList oldSlugs)
  where
    UTCTime postDay _ = postTime
    (year, month, _) = toGregorian postDay
    slug = pack $ takeBaseName postFilename

withApp :: Bool -> (App -> IO a) -> IO a
withApp isDev inner = do
    let root = "content"
    appData <-
        if isDev
            then pure $ loadData root
            else pure <$> loadData root

    let static' t = staticDevel $ root </> t
    appImg <- static' "img"
    appStatic <- static' "static"
    appTorah <- static' "torah"
    appWellKnown <- static' "well-known"
    appMaxi <- static' "maxi"
    appBase <- mkAutoUpdate defaultUpdateSettings
      { updateAction = loadGhcInfo
      , updateFreq = 1000 * 1000 * 60 * 60 -- hourly
      }
    withCurrentRef $ \appShekel -> inner App {..}

main :: IO ()
main = do
  args <- getArgs
  let res = do
        [devprodS, portS] <- Just args
        devprod <-
          case devprodS of
            "dev" -> Just True
            "prod" -> Just False
            _ -> Nothing
        port <- readMaybe portS
        Just (devprod, port)
  case res of
    Nothing -> error "Arguments: <dev/prod> <port number>"
    Just (devprod, port) -> withApp devprod (warp port)

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
