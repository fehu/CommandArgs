-----------------------------------------------------------------------------
--
-- Module      :  CArgs.Optionals
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE DataKinds #-}

module CArgs.Optionals (

  Optional1, OptionalVar

-- * Declare optionals

, optionalFlag
, optional
, variable

-- * Verbosity

, verbArg
, Verbosity(..)

-- * Help

, helpArg

) where

import AList
import CArgs.Descriptors
import CArgs.Parser
import CArgs.Parsers
import CArgs.Parsers.Internal
import CArgs.Verbosity

-----------------------------------------------------------------------------

make :: (DefaultSingleParser v) => ACombinedArgValParser (AList Positional) '[v] v
make = CombinedArgValParser $ CombinedArgValParserSingle singleParser

make' = CombinedArgValParser . CombinedArgValParserSingle

makeVar :: (DefaultSingleParser v) => ACombinedArgValParser (AList Positional) '[v] (VarArg v)
makeVar = CombinedArgValParser $ CombinedArgValParserVar  defaultArgParser


auto :: (DefaultSingleParser t) => Multiline -> AList Positional '[t]
auto descr = Positional "value" singleParser descr :. Nil

-- | Create an optional 'Flag' argument.
optionalFlag shorts longs descr = Optional shorts longs (make' flag) descr Nil

type Optional1   v = Optional '[v] v
type OptionalVar v = Optional '[v] (VarArg v)

-- | Create an optional one-value argument.
optional :: (DefaultSingleParser v) =>
    [Char] -> [String] -> Multiline -> Multiline -> Optional1 v
optional shorts longs descr argDescr = Optional shorts longs make descr (auto argDescr)

-- | Create an argument with variable number of accepted values.
variable :: (DefaultSingleParser v) =>
    [Char] -> [String] -> Multiline -> String -> Multiline -> OptionalVar v
variable shorts longs descr argName argDescr = Optional shorts longs makeVar descr (vArg :. Nil)
    where vArg = Positional (argName ++ "...") singleParser argDescr


-----------------------------------------------------------------------------

-- | Predefined help argument.
helpArg :: OptionalVar Text
helpArg = variable "h" ["help"] ["Show help"] "cmd" ["Commands to show the help for"]

-- | Predefined verbosity argument.
verbArg :: Optional1 Verbosity
verbArg = optional "V" ["verbosity"] ["Set verbosity"] [
    "verbosity level: 0-3", "or 'silent', 'errors', 'warn', 'full'"
    ]

-----------------------------------------------------------------------------



