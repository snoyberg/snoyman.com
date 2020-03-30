{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhcInfo
  ( GhcInfo (..)
  , GhcVersion
  , loadGhcInfo
  ) where

import RIO
import Network.HTTP.Simple
import Data.Yaml
import Data.Version
import Text.Blaze (ToMarkup (..))
import qualified RIO.Text as T
import Data.Aeson

data GhcInfo = GhcInfo
  { giBase :: !Version
  , giCabal :: !Version
  , giWin32 :: !Version
  }

instance FromJSON GhcInfo where
  parseJSON = withObject "GhcInfo" $ \o -> GhcInfo
    <$> o .: "base"
    <*> o .: "Cabal"
    <*> o .: "Win32"

instance ToMarkup Version where
  toMarkup = toMarkup . showVersion

newtype GhcVersion = GhcVersion Version
  deriving (ToMarkup, Eq, Ord)

instance FromJSON GhcVersion where
  parseJSON = withText "GhcVersion" parseGhcVersion
instance FromJSONKey GhcVersion where
  fromJSONKey = FromJSONKeyTextParser parseGhcVersion

parseGhcVersion :: Text -> Parser GhcVersion
parseGhcVersion text =
  case T.stripPrefix "ghc-" text of
    Nothing -> fail $ "No ghc- prefix: " ++ show text
    Just version -> GhcVersion <$> parseJSON (String version)

loadGhcInfo :: IO (Map GhcVersion GhcInfo)
loadGhcInfo = do
  req <- parseRequestThrow "https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/global-hints.yaml"
  bs <- getResponseBody <$> httpBS req
  decodeThrow bs
