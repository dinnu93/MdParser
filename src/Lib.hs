module Lib where

import Control.Monad (void,guard)
import Control.Monad.Combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative ((<|>))

-- Types

type Document = [Block]


-- Block level types

data Block = Heading Int [Inline]
           | Para [Inline]
           | Blockquote [Block]
           | List ListType [[Block]]
           | CodeBlock (Maybe String) String
           | HtmlBlock String
           | HRule
           deriving (Show,Eq)

data ListType = Bullet Char | Numbered NumWrapper Int deriving (Show,Eq)

data NumWrapper = PeriodFollowing | ParenFollowing | ParensAround
                deriving (Show,Eq)

-- Inline types

data Inline = Text String
            | Space
            | SoftBreak
            | LineBreak
            | Emph [Inline]
            | Strong [Inline]
            | Code String
            | Link [Inline] String
            | Image [Inline] String
            deriving (Show,Eq)
            
-- Parser

type Parser = Parsec Void String

parseLine :: Parser String
parseLine = do
  rest <- many (letterChar <|> char ' ')
  newline <- optional eol
  return $ case newline of
    Just _ -> rest ++ " "
    Nothing -> rest
    
parseHeading :: Parser Block
parseHeading = do
  hashes <- some (char '#')
  guard (length hashes <= 6)
  void spaceChar
  str <- parseLine
  return $ Heading (length hashes) [Text str]
