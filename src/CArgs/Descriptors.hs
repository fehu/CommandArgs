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
           , UndecidableInstances
        #-}

module CArgs.Descriptors (

-- * Descriptors

  CArg(..)
, AnyArg(..)
, ArgHelp(..)

, Positional(..)
, Optional(..)

, Flag(..)
, VarArg(..), varArgs

, AnOptional(..)
, Opt(..), fromOpt, FromOptF
, SubArgs

, CArgs(..)
, getCArgs
, getAllCArgs

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


data AnyArg = forall a v . (CArg a v) => AnyArg (a v)


-----------------------------------------------------------------------------

class ArgHelp a where argId   :: a -> String
                      argType :: a -> String
                      argHelp :: a -> Multiline

instance (CArg a v) => ArgHelp (a v) where argId = argName
                                           argType = parseArgType . argValParser
                                           argHelp = argDescription
instance ArgHelp AnyArg where argId (AnyArg a) = argId a
                              argType (AnyArg a) = argType a
                              argHelp (AnyArg a) = argHelp a

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

data Opt = forall v . (Typeable v, Show v)    => Opt' (AnOptional v)
         | forall v vs . (Typeable v, Show v) => Opt  (Optional vs v)


type FromOptF (a :: [*] -> * -> *) b = forall x y . a x y -> b

fromOpt :: FromOptF Optional r -> Opt -> r
fromOpt f (Opt' (AnOptional opt)) = f opt
fromOpt f (Opt opt) = f opt

--optName (Opt (AnOptional opt)) = argName opt
--optName (Opt' opt) = argName opt

data Flag = Flag

data VarArg v  = VarArg [v]

varArgs (VarArg vs) = vs

-----------------------------------------------------------------------------

instance Show Flag where show _ = "!"

instance (Show a) => Show (VarArg a) where show (VarArg l) = show l

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

describePosArgument (Positional n p d)
    | length d > 1 = unwords [n, tName p] : addIndent (replicate 2 ' ') d
    | otherwise    = [unwords $ n : tName p : "\t-- " : d]

describeOptArgument opt = [
      unwords $ argName opt : map abrace aNames
    , replicate 3 ' ' ++ unwords (shortArgs ++ longArgs)
    ] ++ addIndent (replicate 3 ' ') (optDescription opt)
      ++ concatMap (addIndent $ replicate 6 ' ') aDescs ++ [""]
    where (aNames, aDescs) = unzip $ a2List (argName &&& describePosArgument) subArgs
          subArgs = optSubArgs opt
          shortArgs = map (\c -> ['-', c]) $ shortNames opt
          longArgs = map ("--"++) $ longNames opt


addIndent i = map (i++)

abrace s = "<" ++ s ++ ">"

tName p = ":: " ++ parseArgType p

-----------------------------------------------------------------------------


data CArgs lp = CArgs {
      positionalArguments :: AList Positional lp
    , optionalArguments :: [Opt]
    }

-- | Get positional, optional args.
getCArgs :: CArgs lp -> ([AnyArg], [AnyArg])
getCArgs d = (pos, opt)
    where pos = a2List AnyArg (positionalArguments d)
          opt = map anyArgOpt (optionalArguments d)

getAllCArgs :: CArgs lp -> [AnyArg]
getAllCArgs d = let (pos, opt) = getCArgs d in pos ++ opt


anyArgOpt :: Opt -> AnyArg
anyArgOpt = fromOpt AnyArg

-----------------------------------------------------------------------------


