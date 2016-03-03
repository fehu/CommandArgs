-----------------------------------------------------------------------------
--
-- Module      :  CArgs.Parsers.Internal
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , DataKinds
           , TypeOperators
           , FlexibleContexts
           , ConstraintKinds
           , GADTs
        #-}

module CArgs.Parsers.Internal where

import AList
import CArgs.Descriptors
import CArgs.Parser
import CArgs.Values

import Control.Arrow

import Data.Char
import Data.List (find, drop)
import Data.Maybe (isNothing, fromJust)
import Data.Either
import Data.Either.Projections
import Data.Typeable
import Data.Function (on)

import qualified Data.Map as Map

-----------------------------------------------------------------------------

instance CombinedArgValParser SubArgs (CombinedArgValParserStub subs) '[] Flag where
    parseArgCombined (CombinedArgValParserSingle p) Nil = const (Right Flag) &&& id
    combinedParserName (CombinedArgValParserSingle p)   = parseArgType p

instance CombinedArgValParser SubArgs (CombinedArgValParserStub subs) '[] (VarArg a) where
    combinedParserName (CombinedArgValParserVar p) = parseArgType p
    parseArgCombined (CombinedArgValParserVar p) Nil args = (varsParsed, rest)
        where varArgs = takeWhile (not . (`startsWith` "-")) args
              varsParsed = fst $ parseArgValue p varArgs
              rest    = drop (length varArgs) args

type TryDR = EitherDR Multiline

-----------------------------------------------------------------------------

type CanParsePositionals lp = (
    MapAList lp (Positional :-: Identity)
  , MapAList lp (TryDR (Positional :-: Identity))
  , MapAList lp (EitherDR (Maybe Multiline) (Positional :-: Identity))
  )

-- | Parse positional arguments.
parsePositional :: (CanParsePositionals lp) =>
                   AList Positional lp
                -> [String]
                -> Try (AList (Positional :-: Identity) lp)
parsePositional al args = if length args < aLength al
    then Left ["Not enough positional arguments"]
    else toEither . fmap concat . leftProjection $ try
    where tryList = aMap f $ aZip al args
          f (a :<: s) = if s `startsWith` "-"
                        then case r of Left ff -> LeftDR . ff $ argName a
                                       Right v -> RightDR $ a :-: Identity v
                        else LeftDR ["Argument '" ++ argName a ++ "' not provided"]
                where parser = argValParser a
                      (r, _) = parseArgValue parser [s]
          try = eitherDR tryList

-----------------------------------------------------------------------------

-- | Parse optional arguments.
parseOptionals :: [Opt]                       -- ^ Available options.
               -> [String]                    -- ^ Command arguments.
               -> (Multiline, OptionalValues) -- ^ Errors and values.
parseOptionals opts = first concat . second optVals . partitionEithers
                    . purgeRepeating . parseOptionals' opts
      where optVals = OptionalValues . Map.fromList
                    . map (argValueName &&& id)
            purgeRepeating (Right h:t)
                | any repeats t = err : purgeRepeating (filter (not . repeats) t)
                | otherwise     = Right h : purgeRepeating t
                where repeats (Right v) = n == argValueName v
                      repeats _         = False
                      n   = argValueName h
                      err = Left ["Optional argument '" ++ n ++ "' used more than once" ]
            purgeRepeating (h:t) = h : purgeRepeating t
            purgeRepeating [] = []



-- | Try to parse all optional arguments.
parseOptionals' _ [] = []
parseOptionals' [] _ = []
parseOptionals' opts args = let (tried, rest) = parseOptional' opts args
    in case rest of [] -> [tried]
                    _  -> tried : parseOptionals' opts rest

-- | Try to parse some argument.
parseOptional' opts (h:t) | h `startsWith` "--"
                            && length h > 2
                            && (not . any isSpace $ h) =
                                           processOpt (findOptByLong opts) (drop 2 h) "--" t
                          | h `startsWith` "-"
                            && length h == 2
                            && (not . isSpace $ h!!1) =
                                           processOpt (findOptByShort opts) (tail h) "-" t
                          | otherwise = (Left [ "Failed to parse any optional argument, "
                                              ++ "encountered '" ++ h ++ "'"
                                              ], t)

-- | Try to parse given argument.
processOpt :: (String -> Maybe Opt)
          -> String -> String -> [String]
          -> (Try ArgValue, [String])
processOpt find name pref = case find name
    of Just (Opt (AnOptional opt)) -> processOpt' opt
       Just (Opt' opt)             -> processOpt' opt
       _                           -> const (Left [err]) &&& id
    where err = "Unknown optional argument '" ++ pref ++ name ++"'"


-- | Try to parse given argument (2).
processOpt' :: ( Typeable v, Show v ) => Optional vs v -> [String]
                                      -> (Try ArgValue, [String])
processOpt' opt args = (try, t)
    where (parsed, t) = parseArgCombined parser subParsers args
          parser      = optArgVParser opt
          subParsers  = optSubArgs opt
          try = mapLeft ($ argName opt) . mapRight (ArgValue opt) $ parsed



findOptByLong opts name = find (elem name . fromOpt longNames) opts
findOptByShort opts [name] = find (elem name . fromOpt shortNames) opts

(h:t) `startsWith` (ph:pt) = h == ph && t `startsWith` pt
_     `startsWith` []   = True
_     `startsWith` pref = False




