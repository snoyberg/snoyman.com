{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections     #-}
module SnoymanCom
    ( prodMain
    , develMain
    , resourcesApp
    , Widget
    ) where

import ClassyPrelude.Yesod
import Control.Concurrent.Async (race_)
import Text.Hamlet (hamletFile)
import Data.Yaml (decodeFileEither)
import Data.Aeson (withObject, (.:?), withText, (.!=))
import Text.Blaze (ToMarkup (..))
import Text.Markdown
import qualified Data.Vector as V
import Data.Text.Read (decimal)
import Yesod.GitRepo
import System.Directory (doesFileExist, canonicalizePath)
import System.FilePath (takeBaseName, splitExtension, splitPath)
import qualified Data.Map as Map
import Data.Time (diffUTCTime)
import Shekel

data App = App
    { appData   :: GitRepo Data
    , appImg    :: Static
    , appStatic :: Static
    , appTorah  :: Static
    , appWellKnown :: Static
    , appShekel :: !CurrentRef
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
    , homeProfile :: Profile
    , homeToplinks :: Vector Link
    , homeAbout :: Html
    , homePubs :: Vector Pub
    , homeTalks :: Vector Talk
    , homeSites :: Vector Link
    }

renderMarkdown :: Text -> Html
renderMarkdown = markdown def
    { msXssProtect = False
    , msAddHeadingId = True
    } . fromStrict

instance FromJSON Home where
    parseJSON = withObject "Home" $ \o -> Home
        <$> o .: "title"
        <*> o .: "profile"
        <*> o .: "toplinks"
        <*> (renderMarkdown <$> (o .: "about"))
        <*> o .: "publications"
        <*> o .: "talks"
        <*> o .: "sites"

data Profile = Profile
    { profileURL :: Text
    , profileAlt :: Text
    , profileWidth :: Int
    , profileHeight :: Int
    }
instance FromJSON Profile where
    parseJSON = withObject "Profile" $ \o -> Profile
        <$> o .: "url"
        <*> o .: "alt"
        <*> o .: "width"
        <*> o .: "height"
instance ToMarkup Profile where
    toMarkup Profile {..} =
        [shamlet|
            <img
                src=#{profileURL}
                alt=#{profileAlt}
                width=#{profileWidth}
                height=#{profileHeight}
                >
        |]

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
        parseYearMonth t = either fail return $ do
            (year, t1) <- decimal t
            t2 <- maybe (Left "no dash") Right $ stripPrefix "-" t1
            (month, "") <- decimal t2
            when (month < 1 || month > 12) $ Left "Invalid month"
            return $ YearMonth year month
instance ToMarkup Date where
    toMarkup (YearMonth year month') =
        toMarkup monthT ++ " " ++ toMarkup year
      where
        months = asVector $ pack $ words
            $ asText "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"
        monthI
            | month' < 1 = 0
            | month' > 12 = 11
            | otherwise = month' - 1
        monthT = months V.! monthI

data Talk = Talk
    { _talkDate :: Date
    , _talkTitle :: Text
    , _talkVenue :: Text
    , _talkLinks :: Either Text (Vector Link)
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
        | i < 10 = "0" ++ tshow i
        | otherwise = tshow i
    fromPathPiece t
        | length t /= 2 = Nothing
        | Right (i, "") <- decimal t, i >= 1, i <= 12 = Just $ Month i
        | otherwise = Nothing

data Post = Post
    { postTitle :: Text
    , postContent :: Html
    , postTime :: UTCTime
    , postListed :: Bool
    , postFilename :: !FilePath
    , postDescription :: !(Maybe Text)
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/img ImgR Static appImg
/static StaticR Static appStatic
/torah TorahR Static appTorah
/robots.txt RobotsR GET
/favicon.ico FaviconR GET
/reload ReloadR GitRepo-Data appData
/blog BlogR GET
/blog/#Year/#Month/#Text PostR GET
/feed FeedR GET
/.well-known WellKnownR Static appWellKnown
/reveal/*[Text] RevealR GET
/shekel ShekelR GET
/shekel/feed ShekelFeedR GET
|]

instance Yesod App where
    approot = guessApproot
    makeSessionBackend _ = return Nothing

    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
            addScriptRemote "https://code.jquery.com/jquery-3.1.1.slim.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"

            $(whamletFile "templates/default-layout.hamlet")

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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
getData = getYesod >>= liftIO . grContent . appData

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
    reverse . sortOn (postTime . snd) . mapToList <$> getPosts unlisted False

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
    defaultLayout $ do
        setTitle $ toHtml homeTitle
        $(whamletFile "templates/home.hamlet")

prettyDay :: UTCTime -- ^ today, for determining april fools age
          -> UTCTime -- ^ day to show
          -> String
prettyDay today x
  | (year, 4, 1) <- toGregorian (utctDay x)
  , diffUTCTime today x >= (60 * 60 * 24 * 4) = "April Fools', " ++ show year
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
  case lookup (year, month, slug) $ dataPostsAll dat of
    Just (Right slug') -> redirect $ PostR year month slug'
    _ -> return ()

getPostR :: Year -> Month -> Text -> Handler Html
getPostR year month slug = do
    forM_ (stripSuffix ".html" slug) (redirect . PostR year month)
    checkRedirects year month slug
    postsMap <- getPosts True True
    thisPost <- maybe notFound return $ lookup (year, month, slug) postsMap
    posts <- getDescendingPosts False
    now <- liftIO getCurrentTime
    addPreview <- getAddPreview
    defaultLayout $ do
        setTitle $ toHtml $ postTitle thisPost
        forM_ (postDescription thisPost) $ \desc ->
          toWidgetHead [shamlet|<meta name=og:description value=#{desc}>|]
        toWidget
            [lucius|
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
            |]
        $(whamletFile "templates/blog.hamlet")
        toWidget
            [julius|
                hljs.configure({languages:[]});
                hljs.initHighlightingOnLoad();
            |]
        addStylesheetRemote "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.8.0/styles/default.min.css"
        addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.8.0/highlight.min.js"
        addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.4.0/languages/haskell.min.js"

getFeedR :: Handler TypedContent
getFeedR = do
    posts <- take 10 <$> getDescendingPosts False
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
  contentDir <- liftIO $ dataRoot <$> grContent (appData master)
  liftIO $ canonicalizePath $ contentDir </> "reveal"

getRevealR :: [Text] -> Handler Html
getRevealR [] = do
  revealDir <- getRevealDir
  let stripDir x = do
        y <- stripPrefix revealDir x
        Just $ fromMaybe y $ stripPrefix "/" y
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
              $forall (pieces, whole) <- files
                <li>
                  <a href=@{RevealR pieces}>#{whole}
    |]
getRevealR pieces = do
    mapM_ checkPiece pieces
    revealDir <- getRevealDir
    let fp = revealDir </> unpack (intercalate "/" pieces ++ ".md")
    markdown :: [Text] <- (lines . decodeUtf8) <$> readFile fp `catchIO` \_ -> notFound
    let (header, drop 1 -> body) = break (== "---") $ drop 1 markdown
        title = fromMaybe "Reveal.js Slideshow"
              $ listToMaybe
              $ mapMaybe (stripPrefix "title: ") header
        tokenized = tokenize RTNewColumn id body
        tokenize wrapper front [] = [wrapper $ unlines $ front []]
        tokenize wrapper front ("---":rest) = wrapper (unlines $ front []) : tokenize RTNewColumn id rest
        tokenize wrapper front ("----":rest) = wrapper (unlines $ front []) : tokenize RTNewRow id rest
        tokenize wrapper front (x:rest) = tokenize wrapper (front . (x:)) rest
        cols = toCols tokenized
        toCols [] = []
        toCols (RTNewColumn content:rest) =
          let (cols, rest') = getRows id rest
           in (content:cols) : toCols rest'
        toCols (RTNewRow content:rest) = toCols (RTNewColumn content:rest)

        getRows front (RTNewRow content:rest) = getRows (front . (content:)) rest
        getRows front rest = (front [], rest)
    withUrlRenderer $(hamletFile "reveal.hamlet")
  where
    checkPiece t =
      case uncons t of
        Nothing -> notFound
        Just ('.', _) -> notFound
        _ | '/' `elem` t -> notFound
        _ -> return ()

loadData :: FilePath -> IO Data
loadData dataRoot = do
    let readData x y = do
            bs <- readFile $ dataRoot </> y
            return $ TypedContent x $ toContent $ asByteString bs
    dataFavicon <- readData "image/x-icon" "favicon.ico"
    dataRobots <- readData "text/plain" "robots.txt"
    dataHome <- decodeFileEither (dataRoot </> "home.yaml")
            >>= either throwM return

    rawPosts <- decodeFileEither (dataRoot </> "posts.yaml")
            >>= either throwM return
    dataPostsAll <- (mapFromList . concat) <$> mapM (loadPost dataRoot) (asList rawPosts)

    return Data {..}

data PostRaw = PostRaw FilePath Text UTCTime Bool !(Maybe Text) !(Set Text)
instance FromJSON PostRaw where
    parseJSON = withObject "PostRaw" $ \o -> PostRaw
        <$> o .: "file"
        <*> o .: "title"
        <*> ((flip UTCTime 0 <$> (o .: "day")) <|> (o .: "time"))
        <*> o .:? "listed" .!= True
        <*> o .:? "description"
        <*> o .:? "old-slugs" .!= mempty

loadPost :: FilePath -> PostRaw -> IO [((Year, Month, Text), Either Post Text)]
loadPost dataRoot (PostRaw postFilename postTitle postTime postListed postDescription oldSlugs) = do
    postContent <- fmap (renderMarkdown . decodeUtf8) $ readFile $ dataRoot </> postFilename
    let year' = Year $ fromIntegral year
        month' = Month month
    return
      $ ((year', month', slug), Left Post {..})
      : map (\slug' -> ((year', month', slug'), Right slug)) (setToList oldSlugs)
  where
    UTCTime postDay _ = postTime
    (year, month, _) = toGregorian postDay
    slug = pack $ takeBaseName postFilename

withApp :: Bool -> (App -> IO a) -> IO a
withApp isDev inner = do
    appData <- if isDev
        then gitRepoDev
            "content"
            loadData
        else gitRepo
            "https://github.com/snoyberg/snoyman.com-content"
            "master"
            loadData

    root <- dataRoot <$> grContent appData

    let static' t = staticDevel $ root </> t
    appImg <- static' "img"
    appStatic <- static' "static"
    appTorah <- static' "torah"
    appWellKnown <- static' "well-known"
    withCurrentRef $ \appShekel -> inner App {..}

prodMain :: IO ()
prodMain = withApp False warpEnv

develMain :: IO ()
develMain =
  withApp True $
  Control.Concurrent.Async.race_ watchTermFile . warpEnv

-- | Would certainly be more efficient to use fsnotify, but this is
-- simpler.
watchTermFile :: IO ()
watchTermFile =
    loop
  where
    loop = do
        exists <- doesFileExist "yesod-devel/devel-terminate"
        if exists
            then return ()
            else do
                threadDelay 100000
                loop

getShekelR :: Handler ()
getShekelR = do
  currentRef <- appShekel <$> getYesod
  liftIO (htmlRes currentRef) >>= sendWaiResponse

getShekelFeedR :: Handler ()
getShekelFeedR = do
  currentRef <- appShekel <$> getYesod
  liftIO (atomRes currentRef) >>= sendWaiResponse
