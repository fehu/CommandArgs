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

{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , DataKinds
           , TypeOperators
           , FlexibleContexts
        #-}

module CArgs.Parsers (

  DefaultSingleParser(singleParser)

, flag
, text, Text(..)
, int
, float
, bool
, list

, parsePositional

) where

import AList
import CArgs.Descriptors
import CArgs.Parser

import Text.Read
import Data.Char
import Data.Maybe
import Data.List.Split
import Data.Either.Projections


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



-----------------------------------------------------------------------------

instance CombinedArgValParser subs (CombinedArgValParserStub subs) '[] Flag where
    parseArgCombined (CombinedArgValParserSingle p) _ = parseArgValue p
    combinedParserName (CombinedArgValParserSingle p) = parseArgType p


type TryDR = EitherDR Multiline

-----------------------------------------------------------------------------

parsePositional :: ( MapAList lp (Positional :-: Identity)
                   , MapAList lp (TryDR (Positional :-: Identity))
                   , MapAList lp (EitherDR (Maybe Multiline) (Positional :-: Identity))
               ) =>
                   AList Positional lp
                -> [String]
                -> Try (AList (Positional :-: Identity) lp)
parsePositional al args =  toEither . fmap concat . leftProjection $ try
    where tryList = aMap f $ aZip al args
          f (a :<: s) = case r of Left ff -> LeftDR . ff $ argName a
                                  Right v -> RightDR $ a :-: Identity v
                where parser  = argValParser a
                      (r, ls) = parseArgValue parser [s]
          try = eitherDR tryList

-----------------------------------------------------------------------------


parseOptionals = undefined

















