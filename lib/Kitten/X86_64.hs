{-# LANGUAGE OverloadedStrings #-}

module Kitten.X86_64
  ( x86_64
  ) where

import Data.Monoid
import Data.Text (Text)
import Prelude hiding (lines)

import qualified Data.Text as T

import Kitten.Util.Text (showText)
import Kitten.Yarn

x86_64 :: Instruction -> Text
x86_64 instruction = case instruction of
  Act _label _names
    -> "\t; TODO compile act"
  Builtin _builtin
    -> "\t; TODO compile builtin"
  Call label
    -> "\tcall l" <> showText label
  Closure _index -> "\t; TODO compile closure"
  Comment text -> "\t; " <> text
  Enter -> lines
    [ ["push", "rax"]
    , ["mov", "rax,[rbx]"]
    , ["add", "rbx,8"]
    ]
  EntryLabel -> tabs ["; entry"]
  Jump _offset -> tabs ["; TODO compile jump"]
  JumpIfFalse _offset -> tabs ["; TODO compile jf"]
  JumpIfNone _offset -> tabs ["; TODO compile jn"]
  JumpIfRight _offset -> tabs ["; TODO compile jr"]
  Leave -> tabs ["pop"]
  Label label -> "l" <> showText label <> ":"
  Local index -> lines
    [ ["sub", "rbx,8"]
    , ["mov", "[rbx],rax"]
    , ["mov", "rax,[rsp+" <> showText (index * 8) <> "]"]
    ]
  MakeVector _size -> tabs ["; TODO compile vector"]
  Push _value -> tabs ["; TODO compile push"]
  Return -> tabs ["ret"]

lines :: [[Text]] -> Text
lines = T.intercalate "\n" . map tabs

tabs :: [Text] -> Text
tabs = ("\t" <>) . T.intercalate "\t"
