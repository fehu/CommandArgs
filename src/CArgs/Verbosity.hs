-----------------------------------------------------------------------------
--
-- Module      :  CArgs.Verbosity
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module CArgs.Verbosity (

  Verbosity(..)

) where

import CArgs.Descriptors

import Data.List (intercalate)
import qualified GHC.Read as Read

-----------------------------------------------------------------------------



data Verbosity = VerbSilent | VerbErrors | VerbWarn |  VerbFull

instance Show Verbosity where show VerbSilent = "silent"
                              show VerbErrors = "errors"
                              show VerbWarn   = "warn"
                              show VerbFull   = "full"


instance Read Verbosity where readPrec = Read.parens
                                       $ Read.choose [
                                            ("0",      return VerbSilent)
                                          , ("silent", return VerbSilent)
                                          , ("1",      return VerbErrors)
                                          , ("errors", return VerbErrors)
                                          , ("2",      return VerbWarn)
                                          , ("warn",   return VerbWarn)
                                          , ("3",      return VerbFull)
                                          , ("full",   return VerbFull)
                                       ]

-----------------------------------------------------------------------------
