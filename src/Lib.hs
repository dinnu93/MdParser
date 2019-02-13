module Lib where

import Control.Monad (void)
import Control.Monad.Combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)

-- Types

type Document = [Block]


-- Block level types

data Block = Heading Int [Inline]
           | Para [Inline]
           | Blockquote [Block]
           | List ListType [[Block]]
           | CodeBlock (Maybe Text) Text
           | HtmlBlock Text
           | HRule
           deriving (Show,Eq)

-- Inline types

data Inline = Str Text
            | Space
            | SoftBreak
            | LineBreak
            | Emph [Inline]
            | Strong [Inline]
            | Code Text
            | Link [Inline] Text
            | Image [Inline] Text
            deriving Show
            
-- Parser

type Parser = Parsec Void String

