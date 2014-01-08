{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Util.Text
  ( Format(..)
  , ToText(..)
  , escapeHtml
  , readFileUtf8
  , showText
  , toHtml
  , toVerbose
  , module Data.Text
  ) where

import Control.Applicative
import Data.Text
import Data.Text.Encoding

import qualified Data.ByteString as B
import qualified Data.Text as T

data Format = Plain | Verbose | Html

class ToText a where
  toText :: a -> Text
  toText = toTextWith Plain

  toTextWith :: Format -> a -> Text
  toTextWith Plain = toText
  toTextWith Verbose = toText
  toTextWith Html = escapeHtml . toText

escapeHtml :: Text -> Text
escapeHtml = T.concatMap escapeHtmlChar
  where
  escapeHtmlChar = \case
    '<' -> "&lt;"
    '&' -> "&amp;"
    '>' -> "&gt;"
    '"' -> "&quot;"
    c -> T.singleton c

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = decodeUtf8 <$> B.readFile path

showText :: (Show a) => a -> Text
showText = pack . show

toHtml :: (ToText a) => a -> Text
toHtml = toTextWith Html

toVerbose :: (ToText a) => a -> Text
toVerbose = toTextWith Verbose
