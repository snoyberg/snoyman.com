{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
import           ClassyPrelude.Yesod
import Text.Hamlet (hamletFile)
import Data.Yaml (decodeFileEither)
import Data.Aeson (withObject, (.:?), withText)
import Text.Lucius (luciusRT)
import Text.Blaze (ToMarkup (..))
import Yesod.Static
import Text.Markdown
import qualified Data.Vector as V
import Data.Text.Read (decimal)
import Yesod.GitRepo
import Yesod.Feed
import System.FilePath (takeBaseName)
import System.Environment (lookupEnv)

data App = App
    { appData   :: GitRepo Data
    , appImg    :: Static
    , appStatic :: Static
    , appTorah  :: Static
    }

data Data = Data
    { dataHome :: Home
    , dataRootStyle :: TypedContent
    , dataBlogStyle :: TypedContent
    , dataRoot :: FilePath
    , dataRobots :: TypedContent
    , dataFavicon :: TypedContent
    , dataPosts :: Map (Year, Month, Text) Post
    }

data Home = Home
    { homeTitle :: Text
    , homeProfile :: Profile
    , homeToplinks :: Vector Link
    , homeAbout :: Markdown
    , homePubs :: Vector Pub
    , homeTalks :: Vector Talk
    , homeSites :: Vector Link
    }

instance FromJSON Home where
    parseJSON = withObject "Home" $ \o -> Home
        <$> o .: "title"
        <*> o .: "profile"
        <*> o .: "toplinks"
        <*> (Markdown <$> (o .: "about"))
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
    { linkURL :: Text
    , linkText :: Text
    }
instance FromJSON Link where
    parseJSON = withObject "Link" $ \o -> Link <$> o .: "url" <*> o .: "text"
instance ToMarkup Link where
    toMarkup (Link u t) = [shamlet|<a href=#{u}>#{t}|]

data Pub = Pub
    { pubURL :: Text
    , pubText :: Text
    , pubDate :: Date
    , pubPublisher :: Maybe Text
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
        | i < 10 = "0" ++ tshow i
        | otherwise = tshow i
    fromPathPiece t
        | length t /= 2 = Nothing
        | Right (i, "") <- decimal t, i >= 1, i <= 12 = Just $ Month i

data Post = Post
    { postTitle :: Text
    , postContent :: Markdown
    , postDay :: Day
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/style.lucius RootStyleR GET
/blog-style.lucius BlogStyleR GET
/img ImgR Static appImg
/static StaticR Static appStatic
/torah TorahR Static appTorah
/robots.txt RobotsR GET
/favicon.ico FaviconR GET
/reload ReloadR GitRepo-Data appData
/blog PostsR GET
/blog/#Year/#Month/#Text PostR GET
/feed FeedR GET
|]

instance Yesod App where
    makeSessionBackend _ = return Nothing

getData :: Handler Data
getData = getYesod >>= liftIO . grContent . appData

getHomeR :: Handler Html
getHomeR = do
    Home {..} <- dataHome <$> getData
    withUrlRenderer $(hamletFile "templates/home.hamlet")

getRootStyleR :: Handler TypedContent
getRootStyleR = dataRootStyle <$> getData

getBlogStyleR :: Handler TypedContent
getBlogStyleR = dataBlogStyle <$> getData

getFaviconR :: Handler TypedContent
getFaviconR = dataFavicon <$> getData

getRobotsR :: Handler TypedContent
getRobotsR = dataRobots <$> getData

sidebar :: renderer -> Html
sidebar = $(hamletFile "templates/sidebar.hamlet")

getPostsR :: Handler Html
getPostsR = do
    posts <- dataPosts <$> getData
    withUrlRenderer $(hamletFile "templates/posts.hamlet")

getPostR :: Year -> Month -> Text -> Handler Html
getPostR year month slug = do
    forM_ (stripSuffix ".html" slug) (redirect . PostR year month)
    posts <- dataPosts <$> getData
    Post {..} <- maybe notFound return $ lookup (year, month, slug) posts
    let archive = []
    withUrlRenderer $(hamletFile "templates/blog.hamlet")

getFeedR :: Handler TypedContent
getFeedR = do
    posts <- take 10 . reverse . mapToList . dataPosts <$> getData
    updated <-
        case posts of
            [] -> notFound
            (_, post):_ -> return $ postDay post
    newsFeed Feed
        { feedTitle = "Michael Snoyman's blog"
        , feedLinkSelf = FeedR
        , feedLinkHome = HomeR
        , feedAuthor = "Michael Snoyman"
        , feedDescription = "Michael's thoughts on everything, mostly Haskell and tech startups"
        , feedLanguage = "en"
        , feedUpdated = UTCTime updated 0
        , feedLogo = Nothing
        , feedEntries = flip map posts $ \((year, month, slug), post) -> FeedEntry
            { feedEntryLink = PostR year month slug
            , feedEntryUpdated = UTCTime (postDay post) 0
            , feedEntryTitle = postTitle post
            , feedEntryContent = toHtml (postContent post)
            , feedEntryEnclosure = Nothing
            }
        }

loadData :: FilePath -> IO Data
loadData dataRoot = do
    let readData x y = do
            bs <- readFile $ dataRoot </> y
            return $ TypedContent x $ toContent $ asByteString bs
    dataFavicon <- readData "image/x-icon" "favicon.ico"
    dataRobots <- readData "text/plain" "robots.txt"
    dataHome <- decodeFileEither (dataRoot </> "home.yaml")
            >>= either throwM return
    dataRootStyle <- do
        txt <- readFile $ dataRoot </> "style.lucius"
        rendered <- either error return $ luciusRT txt []
        return $ TypedContent typeCss $ toContent txt

    dataBlogStyle <- do
        txt <- readFile $ dataRoot </> "blog-style.lucius"
        rendered <- either error return $ luciusRT txt []
        return $ TypedContent typeCss $ toContent txt

    rawPosts <- decodeFileEither (dataRoot </> "posts.yaml")
            >>= either throwM return
    dataPosts <- mapFromList <$> mapM (loadPost dataRoot) rawPosts

    return Data {..}

data PostRaw = PostRaw FilePath Text Day
instance FromJSON PostRaw where
    parseJSON = withObject "PostRaw" $ \o -> PostRaw
        <$> o .: "file"
        <*> o .: "title"
        <*> (o .: "day" >>= maybe (fail "invalid day") return . readMay . asText)

loadPost :: FilePath -> PostRaw -> IO ((Year, Month, Text), Post)
loadPost dataRoot (PostRaw suffix postTitle postDay) = do
    postContent <- fmap Markdown $ readFile $ dataRoot </> suffix
    return ((Year $ fromIntegral year, Month month, slug), Post {..})
  where
    (year, month, _) = toGregorian postDay
    slug = pack $ takeBaseName suffix

main :: IO ()
main = do
    isDev <- fmap isJust (lookupEnv "DEV")
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
    warpEnv App {..}
