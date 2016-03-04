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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module CArgs.Parsers (

  DefaultSingleParser(singleParser)
, DefaultArgParser(defaultArgParser)

, flag
, text
, int
, float
, bool
, list

, varArg


, Text(..), text2str

) where

import CArgs.Descriptors
import CArgs.Parser
import CArgs.Verbosity

import Text.Read
import Data.Char
import Data.Maybe
import Data.List.Split (splitOneOf)


-----------------------------------------------------------------------------

newtype Text = Text String
instance Show Text where show (Text s) = show s

text2str (Text s) = s

-----------------------------------------------------------------------------

class DefaultSingleParser v where singleParser :: SingleParser v


-- Defaults:

instance DefaultSingleParser Flag   where singleParser = flag
instance DefaultSingleParser Text   where singleParser = text
instance DefaultSingleParser Int    where singleParser = int
instance DefaultSingleParser Float  where singleParser = float
instance DefaultSingleParser Bool   where singleParser = bool

instance (DefaultSingleParser a) =>
    DefaultSingleParser [a] where singleParser = list singleParser

instance DefaultSingleParser Verbosity where
    singleParser = SingleParser "Verbosity" readMaybe

-----------------------------------------------------------------------------

class DefaultArgParser v where defaultArgParser :: AnArgValParser v

instance (DefaultSingleParser a) =>
    DefaultArgParser (VarArg a) where defaultArgParser = varArg singleParser


-----------------------------------------------------------------------------
-- Parsers


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
list (SingleParser name parse) = SingleParser name f
    where name = "List " ++ name
          f x = case map parse $ splitOneOf ",;" x
                    of l | all isJust l -> Just $ map fromJust l
                       _                -> Nothing


-----------------------------------------------------------------------------


varArg :: SingleParser a -> AnArgValParser (VarArg a)
varArg p = ArgValParser . GenericParser ("VarArg " ++ show p) $
    \varArgs -> let varsParsed' = map (parseSingle p) varArgs
                    varsParsed  = if any isNothing varsParsed'
                                   then Left $ \a -> [ "Failed to parse vararg optional attribute '"
                                                     ++ a ++ "', encountered" ++ show varArgs ]
                                   else Right . VarArg $ map fromJust varsParsed'
                in (varsParsed, [])











