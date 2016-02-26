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


{-# LANGUAGE DataKinds #-}

module CArgs (

  module CArgs.Descriptors
, module CArgs.Parsers

, optionalFlag
, optional

) where


import CArgs.Descriptors
import CArgs.Parser
import CArgs.Parsers

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


