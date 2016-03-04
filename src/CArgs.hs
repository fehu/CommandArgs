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


{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module CArgs (

    application
  , parseArgs

  , AList(..)
  , SingleParser(..)
  , GenericParser(..)

  , module X
)
  where

import CArgs.Descriptors as X
import CArgs.Parsers as X
import CArgs.Optionals as X
import CArgs.Values as X
import CArgs.Handler as X


import AList
import CArgs.Parser
import CArgs.Parsers.Internal

import Data.Either (isLeft)

import System.Environment

-----------------------------------------------------------------------------

parseArgs :: (CanParsePositionals lp) => CArgs lp -> [String] -> CArgValues lp
parseArgs d args = CArgValues positionalVals oVals oErrs
    where positionals     = positionalArguments d
          positionalsLen  = aLength positionals
          positionalVals  = parsePositional positionals args
          argsRest        = if isLeft positionalVals then args else drop positionalsLen args
          (oErrs, oVals)  = parseOptionals (optionalArguments d) argsRest


-----------------------------------------------------------------------------

-- | Parses current executable's command arguments and proceeds according to
--   given 'ArgsHandler'
application :: (CanParsePositionals lp) => CArgs lp -> ArgsHandler lp -> IO ()
application ca ah = getArgs >>= (handleArgs ah . parseArgs ca)




