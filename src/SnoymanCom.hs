{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, splitExtension, splitPath)
import qualified Data.Map as Map

data App = App
    { appData   :: GitRepo Data
    , appImg    :: Static
    , appStatic :: Static
    , appTorah  :: Static
    , appWellKnown :: Static
    }

data Data = Data
    { dataHome :: Home
    , dataRoot :: FilePath
    , dataRobots :: TypedContent
    , dataFavicon :: TypedContent
    , dataPostsAll :: Map (Year, Month, Text) Post
    }

dataPostsPast :: MonadIO m
              => Data
              -> m (Map (Year, Month, Text) Post)
dataPostsPast d = do
    now <- liftIO getCurrentTime
    return $ Map.filter ((<= now) . postTime) (dataPostsAll d)

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
         -> Handler (Map (Year, Month, Text) Post)
getPosts unlisted = do
    mpreview <- lookupGetParam "preview"
    dat <- getData
    posts <-
        case mpreview of
            Just "true" -> return $ dataPostsAll dat
            _ -> dataPostsPast dat
    return $
        if unlisted
            then posts
            else Map.filter postListed posts

getDescendingPosts :: Bool -- ^ include unlisted?
                   -> Handler [((Year, Month, Text), Post)]
getDescendingPosts unlisted =
    reverse . sortOn (postTime . snd) . mapToList <$> getPosts unlisted

getHomeR :: Handler Html
getHomeR = do
    dat <- getData
    posts <- getDescendingPosts False
    let Home {..} = dataHome dat
        mRecentPost =
            case posts of
                ((year, month, slug), post):_ -> Just
                    ( PostR year month slug
                    , postTitle post
                    , prettyDay $ postTime post
                    )
                [] -> Nothing
    defaultLayout $ do
        setTitle $ toHtml homeTitle
        $(whamletFile "templates/home.hamlet")

prettyDay :: UTCTime -> String
prettyDay x
  | (2017, 4, 1) <- toGregorian (utctDay x) = "5 Nissan, 5777"
  | otherwise = formatTime defaultTimeLocale "%B %e, %Y" x

getFaviconR :: Handler TypedContent
getFaviconR = dataFavicon <$> getData

getRobotsR :: Handler TypedContent
getRobotsR = dataRobots <$> getData

getBlogR :: Handler ()
getBlogR = do
    posts <- getDescendingPosts False
    case posts of
        [] -> notFound
        ((year, month, slug), _):_ -> redirect $ PostR year month slug

getPostR :: Year -> Month -> Text -> Handler Html
getPostR year month slug = do
    forM_ (stripSuffix ".html" slug) (redirect . PostR year month)
    postsMap <- getPosts True
    thisPost <- maybe notFound return $ lookup (year, month, slug) postsMap
    posts <- getDescendingPosts False
    defaultLayout $ do
        setTitle $ toHtml $ postTitle thisPost <> " - Michael Snoyman's blog"
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

getRevealR :: [Text] -> Handler Html
getRevealR [] = do
  files <- runConduit
         $ sourceDirectoryDeep False "content/reveal"
        .| concatMapC (\fp ->
            case splitExtension fp of
              (("content/reveal/" `stripPrefix`) -> Just x, ".md") ->
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
    let fp = unpack $ intercalate "/" ("content":"reveal":pieces) ++ ".md"
    markdown :: [Text] <- (lines . decodeUtf8) <$> readFile fp `catchIO` \_ -> notFound
    let (header, drop 1 -> body) = break (== "---") $ drop 1 markdown
        title = fromMaybe "Reveal.js Slideshow"
              $ listToMaybe
              $ mapMaybe (stripPrefix "title: ") header
        tokenized = tokenize id body
        tokenize front [] = [RTNewRow $ unlines $ front []]
        tokenize front ("---":rest) = RTNewColumn (unlines $ front []) : tokenize id rest
        tokenize front ("----":rest) = RTNewRow (unlines $ front []) : tokenize id rest
        tokenize front (x:rest) = tokenize (front . (x:)) rest
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
    dataPostsAll <- mapFromList <$> mapM (loadPost dataRoot) rawPosts

    return Data {..}

data PostRaw = PostRaw FilePath Text UTCTime Bool
instance FromJSON PostRaw where
    parseJSON = withObject "PostRaw" $ \o -> PostRaw
        <$> o .: "file"
        <*> o .: "title"
        <*> ((flip UTCTime 0 <$> (o .: "day")) <|> (o .: "time"))
        <*> o .:? "listed" .!= True

loadPost :: FilePath -> PostRaw -> IO ((Year, Month, Text), Post)
loadPost dataRoot (PostRaw suffix postTitle postTime postListed) = do
    postContent <- fmap (renderMarkdown . decodeUtf8) $ readFile $ dataRoot </> suffix
    return ((Year $ fromIntegral year, Month month, slug), Post {..})
  where
    UTCTime postDay _ = postTime
    (year, month, _) = toGregorian postDay
    slug = pack $ takeBaseName suffix

mkApp :: Bool -> IO App
mkApp isDev = do
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
    return App {..}

prodMain :: IO ()
prodMain = mkApp False >>= warpEnv

develMain :: IO ()
develMain = mkApp True >>= race_ watchTermFile . warpEnv

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
