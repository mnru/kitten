module Kitten.Util.Text
  ( ToText(..)
  , readFileUtf8
  , showText
  , module Data.Text
  ) where

import Control.Applicative
import Data.Text
import Data.Text.Encoding

import qualified Data.ByteString as B

class ToText a where
  toText :: a -> Text

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = decodeUtf8 <$> B.readFile path

showText :: (Show a) => a -> Text
showText = pack . show
