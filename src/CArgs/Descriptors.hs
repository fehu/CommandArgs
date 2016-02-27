-----------------------------------------------------------------------------
--
-- Module      :  CArgs.Descriptors
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE ExistentialQuantification
           , MultiParamTypeClasses
           , FlexibleInstances
           , KindSignatures
           , DataKinds
           , Rank2Types
        #-}

module CArgs.Descriptors (

-- * Descriptors

  CArg(..)
, Positional(..)
, Optional(..)
, Flag(..)

, AnOptional(..)
, Opt(..), fromOpt, FromOptF
, SubArgs

, CArgs(..)

-- * misc

, Multiline
, AList(..)

) where


import AList
import CArgs.Parser

import Data.Typeable
import Control.Arrow

-----------------------------------------------------------------------------

type Multiline = [String]

class CArg a v where
    argName :: a v -> String
    argDescription :: a v -> Multiline
    argValParser :: a v -> AnArgValParser v

-----------------------------------------------------------------------------



data Positional a = Positional String (SingleParser a) Multiline

type SubArgs = AList Positional

data Optional vs v = Optional {
      shortNames     :: [Char]
    , longNames      :: [String]
    , optArgVParser  :: ACombinedArgValParser (AList Positional) vs v
    , optDescription :: Multiline
    , optSubArgs     :: SubArgs vs
    }

-----------------------------------------------------------------------------


data AnOptional v = forall vs . AnOptional (Optional vs v)

data Opt = forall v . (Typeable v, Show v)    => Opt  (AnOptional v)
         | forall v vs . (Typeable v, Show v) => Opt' (Optional vs v)


type FromOptF (a :: [*] -> * -> *) b = forall x y . a x y -> b

fromOpt :: FromOptF Optional r -> Opt -> r
fromOpt f (Opt (AnOptional opt)) = f opt
fromOpt f (Opt' opt) = f opt

--optName (Opt (AnOptional opt)) = argName opt
--optName (Opt' opt) = argName opt

data Flag = Flag

-----------------------------------------------------------------------------

instance Show Flag where show _ = "!"

instance Show (Optional vs v) where show = argName

instance Show (Positional  v) where show (Positional name _ _) = name

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


data CArgs lp = CArgs {
      positionalArguments :: AList Positional lp
    , optionalArguments :: [Opt]
    }


-----------------------------------------------------------------------------

