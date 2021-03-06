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

{-# LANGUAGE DataKinds, TypeFamilies #-}

module CArgs.Optionals (

  Optional1
, OptionalT2, OptionalT3
, OptionalVar

-- * Declare optionals

, optionalFlag
, optional
, optional2, optional2'
, optional3, optional3'
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


autoSingle :: (DefaultSingleParser t) => Multiline -> AList Positional '[t]
autoSingle descr = Positional "value" singleParser descr :. Nil

-- | Create an optional 'Flag' argument.
optionalFlag shorts longs descr = Optional shorts longs (make' flag) descr Nil

type Optional1   v   = Optional '[v] v

type OptionalT2 a b   = Optional [a,b] (a,b)
type OptionalT3 a b c = Optional [a,b,c] (a,b,c)

type OptionalVar v = Optional '[v] (VarArg v)

-- | Create an optional one-value argument.
optional :: (DefaultSingleParser v) =>
    [Char] -> [String] -> Multiline -> Multiline -> Optional1 v
optional shorts longs descr argDescr = Optional shorts longs make descr (autoSingle argDescr)

-- | Create an optional 2-value argument.
optional2 :: (DefaultSingleParser v1, DefaultSingleParser v2) =>
             [Char] -> [String] -> Multiline
          -> String -> Multiline
          -> String -> Multiline
          -> (v1 -> v2 -> v)
          -> Optional [v1,v2] v
optional2 shorts longs descr aName1 aDescr1 aName2 aDescr2 combine =
    Optional shorts longs cparser descr parsers
    where
          cparser = CombinedArgValParser $ CombinedArgValParser2 "SomeCombined2" splitSubs combine
          splitSubs :: AList Positional [v1,v2] -> (SingleParser v1, SingleParser v2)
          splitSubs (Positional _ p1 _ :. Positional _ p2 _ :. Nil) = (p1, p2)
          parsers = Positional aName1 singleParser aDescr1
                 :. Positional aName2 singleParser aDescr2
                 :. Nil


optional2' shorts longs descr aName1 aDescr1 aName2 aDescr2 =
        optional2 shorts longs descr aName1 aDescr1 aName2 aDescr2 (,)

-- | Create an optional 3-value argument.
optional3 :: (DefaultSingleParser v1, DefaultSingleParser v2, DefaultSingleParser v3) =>
             [Char] -> [String] -> Multiline
          -> String -> Multiline
          -> String -> Multiline
          -> String -> Multiline
          -> (v1 -> v2 -> v3 -> v)
          -> Optional [v1,v2,v3] v
optional3 shorts longs descr aName1 aDescr1
                             aName2 aDescr2
                             aName3 aDescr3 combine =
    Optional shorts longs cparser descr parsers
    where
          cparser = CombinedArgValParser $ CombinedArgValParser3 "SomeCombined3" splitSubs combine
          splitSubs :: AList Positional [v1,v2,v3] -> (SingleParser v1, SingleParser v2, SingleParser v3)
          splitSubs (Positional _ p1 _ :. Positional _ p2 _ :. Positional _ p3 _ :. Nil) = (p1, p2, p3)
          parsers = Positional aName1 singleParser aDescr1
                 :. Positional aName2 singleParser aDescr2
                 :. Positional aName3 singleParser aDescr3
                 :. Nil


optional3' shorts longs descr aName1 aDescr1
                              aName2 aDescr2
                              aName3 aDescr3 =
        optional3 shorts longs descr aName1 aDescr1 aName2 aDescr2 aName3 aDescr3 (,,)

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



