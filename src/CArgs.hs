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
           , KindSignatures
           , TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , ExistentialQuantification
           , FlexibleContexts
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

, parsePositional

) where

import Control.Arrow

import Data.Typeable
import Data.Either.Projections
import Data.Map (Map)
import qualified Data.Map as Map

import AList

--import GHC.Exts

-----------------------------------------------------------------------------

type Multiline = [String]

class CArg a v where
    argName :: a v -> String
    argDescription :: a v -> Multiline
    argValParser :: a v -> AnArgValParser v

-----------------------------------------------------------------------------

type Try  a = Either Multiline a
type TryA a = Either (String -> Multiline) a

class ArgValParser p v where parseArgValue :: p v -> [String] -> (TryA v, [String])
                             parseArgType  :: p v -> String

data AnArgValParser v = forall p . (ArgValParser p v) => ArgValParser (p v)

data SingleParser v = SingleParser String (String -> Maybe v)

parseSingle (SingleParser _ p) = p

instance Show (AnArgValParser a) where show (ArgValParser p) = parseArgType p
instance Show (SingleParser a) where show (SingleParser name _) = name

instance ArgValParser AnArgValParser v where
    parseArgValue (ArgValParser p) = parseArgValue p
    parseArgType  (ArgValParser p) = parseArgType p


instance ArgValParser SingleParser v where
    parseArgValue (SingleParser _ p) (h:t) =
        case p h of Just v -> (Right v, t)
                    _      -> (Left $ msg h, h:t)
        where msg s arg = ["Failed to parse argument '" ++ arg
                        ++ "', unexpected " ++ s]
    parseArgType  (SingleParser t _) = t


class CombinedArgValParser p vs v
    where parseArgCombined :: p vs v
                           -> SubArgs vs
                           -> [String]
                           -> (TryA v, [String])
          combinedParserName :: p vs v -> String

data ACombinedArgValParser vs v =
    forall p . (CombinedArgValParser p vs v) => CombinedArgValParser (p vs v)

data CombinedArgValParserStub (vs :: [*]) v =
    CombinedArgValParserSingle (SingleParser v)
--  | CombinedArgValParserVar    ([String] -> (Maybe v, String))

instance Show (ACombinedArgValParser vs v) where
    show (CombinedArgValParser p) = combinedParserName p

instance CombinedArgValParser ACombinedArgValParser vs v where
    parseArgCombined (CombinedArgValParser p) = parseArgCombined p
    combinedParserName (CombinedArgValParser p) = combinedParserName p

instance CombinedArgValParser CombinedArgValParserStub '[] Flag where
    parseArgCombined (CombinedArgValParserSingle p) _ l = parseArgValue p l
    combinedParserName (CombinedArgValParserSingle p) = parseArgType p

instance CombinedArgValParser CombinedArgValParserStub '[v] v where
    parseArgCombined (CombinedArgValParserSingle p) _ l = parseArgValue p l
    combinedParserName (CombinedArgValParserSingle p) = parseArgType p

data ArgValParserCombination v =
     ArgValParserCombination String ([String] -> (TryA v, [String]))

instance ArgValParser ArgValParserCombination v where
    parseArgValue (ArgValParserCombination _ p) = p
    parseArgType  (ArgValParserCombination t _) = t

-----------------------------------------------------------------------------

data Positional a = Positional String (SingleParser a) Multiline
    deriving Show

type SubArgs vs = AList Positional vs

data Optional vs v = Optional {
      shortNames     :: [Char]
    , longNames      :: [String]
    , optArgVParser  :: ACombinedArgValParser vs v
    , optDescription :: Multiline
    , optSubArgs     :: SubArgs vs
    }
    deriving Show

data AnOptional v = forall vs . AnOptional (Optional vs v)

data Opt = forall v . (Typeable v)    => Opt  (AnOptional v)
         | forall v vs . (Typeable v) => Opt' (Optional vs v)


data Flag = Flag


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


---- | Transforms an 'AList' of 'TryDR' into a 'Try' of 'AList'.
--tryEitherDR :: (MapAList l r, MapAList l (EitherDR (Maybe Multiline) r)) =>
--            AList (TryDR r) l -> Try (AList r l)
--tryEitherDR = toEither . fmap concat . lp
--    where lp = leftProjection . condMap fm fa
--          fm = rightDR
--          fa = leftDR


--aCollect :: DepFuncM Maybe a b  -- ^ Underlying value optional transformation.
--         -> AList a l           -- ^ AList to transform.
--         -> AnyFunc a Multiline -- ^ Alternative (failure) transformation.
--         -> Try (AList r l)
--aCollect = undefined



t :: AList Maybe '[String]
t = Just "A" :. Nil

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

type TryDR = EitherDR Multiline

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

--            case parseArgValue parser s of Just v -> RightDR $ a :-: Identity v
--                                              _      -> LeftDR [positionalArgNotFound a]


--positionalArgNotFound a = "[ARG ERROR] Positional argument '" ++ argName a ++ "' not specified."

--parsePositional (h:.t) (ah:at) acc =
--    case parseArgValue ah of Just v -> parsePositional t at
--                                     $ Pair h v :. acc
--    Pair h v :. parsePositional t at

--parseArgs :: CArgs lp -> [String] -> Try (CArgValues lp)
--parseArgs ca args =




-----------------------------------------------------------------------------









