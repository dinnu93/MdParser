module Lib where

import Control.Monad 
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
           | Blank
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

parseDocument :: Parser Document
parseDocument = parseBlock `sepBy` ((string "\n\n") <|> (string "\n"))

parseBlock :: Parser Block
parseBlock = try parseHeading <|> parseParagraph

parseParagraph :: Parser Block
parseParagraph = do
  str <- some (anySingleBut '\n')
  return $ Para [Text str]

parseHeading :: Parser Block
parseHeading = do
  hashes <- some (char '#')
  guard (length hashes <= 6)
  void spaceChar
  str <- some (anySingleBut '\n')
  return $ Heading (length hashes) [Text str]
