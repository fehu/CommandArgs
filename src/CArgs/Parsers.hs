-----------------------------------------------------------------------------
--
-- Module      :  CArgs.Parsers
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module CArgs.Parsers (

  DefaultSingleParser(singleParser)

, flag
, text, Text(..)
, int
, float
, bool
, list



) where

import CArgs.Descriptors
import CArgs.Parser

import Text.Read
import Data.Char
import Data.Maybe
import Data.List.Split (splitOneOf)

-----------------------------------------------------------------------------

class DefaultSingleParser v where singleParser :: SingleParser v

newtype Text = Text String deriving Show

-- Defaults:

instance DefaultSingleParser Flag   where singleParser = flag
instance DefaultSingleParser Text   where singleParser = text
instance DefaultSingleParser Int    where singleParser = int
instance DefaultSingleParser Float  where singleParser = float
instance DefaultSingleParser Bool   where singleParser = bool
instance (DefaultSingleParser a) =>
    DefaultSingleParser [a] where singleParser = list singleParser


-----------------------------------------------------------------------------
-- Parsers

--make name =  name


flag = SingleParser "Flag" . const $ Just Flag

text = SingleParser "Text" (Just . Text)


int :: SingleParser Int
int = SingleParser "Int" readMaybe


float :: SingleParser Float
float = SingleParser "Float" readMaybe

bool :: SingleParser Bool
bool = SingleParser "Bool" $ \x -> case map toLower x
                                        of "true"  -> Just True
                                           "1"     -> Just True
                                           "y"     -> Just True
                                           "yes"   -> Just True
                                           "on"    -> Just True
                                           "false" -> Just False
                                           "0"     -> Just False
                                           "n"     -> Just False
                                           "no"    -> Just False
                                           "off"   -> Just False
                                           _       -> Nothing

list :: SingleParser a -> SingleParser [a]
list (SingleParser name parse) = SingleParser name undefined
    where name = "List " ++ name
          f x = case map parse $ splitOneOf ",;" x
                    of l | all isJust l -> Just $ map fromJust l
                       _                -> Nothing















