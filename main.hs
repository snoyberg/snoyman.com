{-# LANGUAGE NoImplicitPrelude #-}
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

data App = App
    { appData   :: IORef Data
    , appImg    :: Static
    , appStatic :: Static
    , appTorah  :: Static
    }

data Data = Data
    { dataHome :: Home
    , dataRootStyle :: TypedContent
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

mkYesod "App" [parseRoutes|
/ HomeR GET
/style.lucius RootStyleR GET
/img ImgR Static appImg
/static StaticR Static appStatic
/torah TorahR Static appTorah
/robots.txt RobotsR GET
/favicon.ico FaviconR GET
|]

instance Yesod App where
    makeSessionBackend _ = return Nothing

getData :: Handler Data
getData = getYesod >>= readIORef . appData

getHomeR :: Handler Html
getHomeR = do
    Home {..} <- dataHome <$> getData
    withUrlRenderer $(hamletFile "templates/home.hamlet")

getRootStyleR :: Handler TypedContent
getRootStyleR = dataRootStyle <$> getData

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "content/favicon.ico"

getRobotsR :: Handler ()
getRobotsR = sendFile "text/plain" "content/robots.txt"

main :: IO ()
main = do
    dataHome <- decodeFileEither "content/home.yaml" >>= either throwM return
    dataRootStyle <- do
        txt <- readFile "content/style.lucius"
        rendered <- either error return $ luciusRT txt []
        return $ TypedContent typeCss $ toContent txt
    appData <- newIORef Data {..}
    appImg <- staticDevel "content/img"
    appStatic <- staticDevel "content/static"
    appTorah <- staticDevel "content/torah"
    warpEnv App {..}
