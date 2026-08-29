{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- Tool for ingesting json from https://ilovesprites.com/tools/images-to-atlas
module Data.Atlas
  ( Atlas
  , loadAtlas
  ) where

import SDL.JuicyPixels
import Data.Function (on)
import Data.Char (isDigit)
import Data.List (sortOn, groupBy)
import GHC.Generics
import Data.Map qualified as M
import Data.Map (Map)
import Data.Aeson
import SDL
import Foreign.C.Types (CInt)
import qualified SDL.Video.Renderer as R

type Atlas = Atlas' Texture

data Atlas' a = Atlas
  { _atlasTexture :: a
  , _unAtlas :: Map String [Rectangle CInt]
  }
  deriving stock (Functor, Foldable, Traversable)

instance FromJSON (Atlas' FilePath) where
  parseJSON = withObject ("atlas") $ \obj -> do
    frames <- obj .: "frames"
    img <- obj .: "image"
    pure $ Atlas img $ parseFrames frames


loadAtlas :: R.Renderer -> FilePath -> IO Atlas
loadAtlas r fp = do
  atlas' <- either error pure =<< eitherDecodeFileStrict fp
  traverse (loadJuicyTexture r) atlas'


splitName :: FilePath -> (String, Int)
splitName = fmap (read . mappend "0") . break isDigit


parseFrames :: [Frame FilePath] -> Map String [Rectangle CInt]
parseFrames fs = M.fromList $ do
  group
    <- groupBy (on (==) (fst . filename))
     $ fmap (fmap splitName)
     $ sortOn filename fs
  pure (fst $ filename $ head group, fmap frame $ sortOn (snd . filename) group)


data Frame a = Frame
  { filename :: a
  , frame :: Rectangle CInt
  }
  deriving stock (Generic, Functor)
  deriving anyclass FromJSON


instance FromJSON (Rectangle CInt) where
  parseJSON = withObject "Rectangle" $ \obj ->
    fmap (fmap $ fromIntegral @Int) $
      Rectangle
        <$> (((P .) . V2) <$> obj .: "x" <*> obj .: "y")
        <*> (V2 <$> obj .: "w" <*> obj .: "h")

