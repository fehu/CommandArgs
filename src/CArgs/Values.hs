-----------------------------------------------------------------------------
--
-- Module      :  CArgs.Values
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
           , TypeOperators
       #-}

module CArgs.Values (

  CArgValues(..)
, ArgValue
, OptionalValues(..)
, getArgValue
, get


) where

import AList
import CArgs.Descriptors

import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------------------


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

