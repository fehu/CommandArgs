-----------------------------------------------------------------------------
--
-- Module      :  CArgs
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- | Arguments in form: @p1 p2 ... [optional]@, where optionals are in forms:
--     1. @-c a1 a2 ...@, where c is a char
--     2. @--argument-name a1 a2@
--   The a1, a2, ... arguments must not have symbol @-@, unless escaped.
--


{-# LANGUAGE DataKinds
           , TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , ExistentialQuantification
       #-}

module CArgs (

-- *

  CArg(..)
, Positional(..)
, Optional(..)
, Flag(..)

, AnOptional(..)
, Opt(..)

, CArgs(..)

-- *
, ArgValParser(..)
, AnArgValParser(..)
, SingleParser(..)

, CombinedArgValParser(..)
, CombinedArgValParserStub(..)
, ACombinedArgValParser(..)

, Multiline

, AList(..)



) where

import Control.Arrow

import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map

import CArgs.Parser

import AList

--import GHC.Exts

-----------------------------------------------------------------------------

type Multiline = [String]

class CArg a v where
    argName :: a v -> String
    argDescription :: a v -> Multiline
    argValParser :: a v -> AnArgValParser v

-----------------------------------------------------------------------------



data Positional a = Positional String (SingleParser a) Multiline
    deriving Show

type SubArgs vs = AList Positional vs

data Optional vs v = Optional {
      shortNames     :: [Char]
    , longNames      :: [String]
    , optArgVParser  :: ACombinedArgValParser (AList Positional) vs v
    , optDescription :: Multiline
    , optSubArgs     :: SubArgs vs
    }

-----------------------------------------------------------------------------


data AnOptional v = forall vs . AnOptional (Optional vs v)

data Opt = forall v . (Typeable v)    => Opt  (AnOptional v)
         | forall v vs . (Typeable v) => Opt' (Optional vs v)


data Flag = Flag deriving Show

-----------------------------------------------------------------------------

instance Show (Optional vs v) where show = argName

instance CArg Positional v where argName (Positional n _ _) = n
                                 argDescription = describePosArgument
                                 argValParser (Positional _ p _) = ArgValParser p

instance CArg (Optional as) v where
    argValParser opt = ArgValParser . ArgValParserCombination (combinedParserName p)
                     $ fun
        where p   = optArgVParser opt
              fun = parseArgCombined p (optSubArgs opt)

    argName opt = case longNames opt
                of [] -> case shortNames opt
                        of "" -> error "empty option"
                           n  -> n
                   n:_ -> n
    argDescription = describeOptArgument


-----------------------------------------------------------------------------

describePosArgument (Positional n _ d) | length d > 1 = n:addIndent "  " d
                                       | otherwise    = [unwords $ n:d]

describeOptArgument opt = [
      unwords $ argName opt : map abrace aNames
    , "\t" ++ unwords (shortArgs ++ longArgs)
    , ""
    ] ++ addIndent "\t" (optDescription opt)
      ++ [""]
      ++ concatMap (addIndent "\t  ") aDescs
    where (aNames, aDescs) = unzip $ a2List (argName &&& describePosArgument)
                                            (optSubArgs opt)
          shortArgs = map (\c -> ['-', c]) $ shortNames opt
          longArgs = map ("--"++) $ longNames opt


addIndent i = map (i++)

abrace s = "<" ++ s ++ ">"


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data CArgs lp = CArgs {
      positionalArguments :: AList Positional lp
    , optionalArguments :: [Opt]
    }

data CArgValues lp = CArgValues {
      positionalValues :: AList (Positional :-: Identity) lp
    , optionalValues   :: OptionalValues
    }

-----------------------------------------------------------------------------


data ArgValue = forall a v . (CArg a v, Typeable v) => ArgValue (a v) v

getArgValue :: (Typeable a) => ArgValue -> Maybe a
getArgValue (ArgValue _ v) = cast v

newtype OptionalValues = Optionals (Map String ArgValue)

get :: (Typeable v) => OptionalValues -> Optional vs v -> Maybe v
(Optionals oMp) `get` opt = getArgValue =<< argName opt `Map.lookup` oMp



-----------------------------------------------------------------------------









