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


{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts #-}

module CArgs (

  module CArgs.Descriptors
, module CArgs.Parsers

-- * Declare optionals

, optionalFlag
, optional

-- * Arguments parsing

, parseArgs

) where

import AList
import CArgs.Descriptors
import CArgs.Parser
import CArgs.Parsers
import CArgs.Parsers.Internal
import CArgs.Values

import Data.Either.Projections

-----------------------------------------------------------------------------

make :: (DefaultSingleParser v) => ACombinedArgValParser (AList Positional) '[v] v
make = CombinedArgValParser $ CombinedArgValParserSingle singleParser

make' = CombinedArgValParser . CombinedArgValParserSingle

auto :: (DefaultSingleParser t) => Multiline -> AList Positional '[t]
auto descr = Positional "value" singleParser descr :. Nil

-- | Create an optional 'Flag' argument.
optionalFlag shorts longs descr = Optional shorts longs (make' flag) descr Nil

-- | Create an optional one-value argument.
optional :: (DefaultSingleParser v) =>
    [Char] -> [String] -> Multiline -> Multiline -> AnOptional v
optional shorts longs descr argDescr = AnOptional $
    Optional shorts longs make descr (auto argDescr)

-----------------------------------------------------------------------------

parseArgs :: (CanParsePositionals lp) => CArgs lp -> [String] -> Try (CArgValues lp)
parseArgs d args = mapRight (\positionals -> CArgValues positionals oVals oErrs)
                            (parsePositional positionals args)
    where positionals     = positionalArguments d
          positionalsLen  = aLength positionals
          (oErrs, oVals)  = parseOptionals (optionalArguments d)
                          $ drop positionalsLen args

-----------------------------------------------------------------------------






