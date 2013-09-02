{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kitten.C.Quote
  ( qc
  ) where

import Data.Text (Text)
import Data.Monoid
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse

import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

data Part = Quote Text | Unquote String

instance Lift Text where
  lift text = liftString (T.unpack text)

qc :: QuasiQuoter
qc = QuasiQuoter
  { quoteExp = toExpr . quote ""
  , quotePat = error "Kitten.C.Quote: quotePat"
  , quoteType = error "Kitten.C.Quote: quoteType"
  , quoteDec = error "Kitten.C.Quote: quoteDec"
  }

toExpr :: [Part] -> TH.ExpQ
toExpr [] = [| "" |]
toExpr (Quote a : xs)
  = TH.appE [| (<>) a |] (toExpr xs)
toExpr (Unquote a : xs)
  = TH.appE [| (<>) $(fromExpr a) |] (toExpr xs)

fromExpr :: String -> TH.Q TH.Exp
fromExpr s = case parseExp s of
  Left failure -> TH.reportError failure >> [| "" |]
  Right expr -> return expr

quote :: String -> String -> [Part]
quote a []           = [Quote (T.reverse $ T.pack a)]
quote a ('\\':x:xs)  = quote (x:a) xs
quote a ('\\':[])    = quote ('\\':a) []
quote a ('#':'{':xs) = Quote (T.reverse $ T.pack a) : unquote [] xs
quote a (x:xs)       = quote (x:a) xs

unquote :: String -> String -> [Part]
unquote a []          = [Quote (T.reverse $ T.pack a)]
unquote a ('\\':x:xs) = unquote (x:a) xs
unquote a ('\\':[])   = unquote ('\\':a) []
unquote a ('}':xs)    = Unquote (reverse a) : quote [] xs
unquote a (x:xs)      = unquote (x:a) xs
