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
           , DataKinds
       #-}

module CArgs.Values (

-- * Argument values

  CArgValues(..)
, posValue

, ArgValue(..)
, argValueName
, getArgValue

, OptionalValues(..)
, get
, flagSet

, optValCount

) where

import AList
import CArgs.Descriptors

import Data.Typeable
import Data.List (intercalate)

import Data.Maybe (isJust)
import Data.Map (Map)
import qualified Data.Map as Map


-----------------------------------------------------------------------------


data CArgValues lp = CArgValues {
      positionalValues :: Either [String] (AList (Positional :-: Identity) lp)
    , optionalValues   :: OptionalValues
    , optionalErrors   :: Multiline
    }
    deriving Show


posValue :: (Positional :-: Identity) v -> v
posValue (_ :-: x) = unwrapId x

-----------------------------------------------------------------------------


data ArgValue = forall a v . (CArg a v, Typeable v, Show v) => ArgValue (a v) v

instance Show ArgValue where show (ArgValue a v) = argName a ++ "=" ++ show v

argValueName (ArgValue a _) = argName a

getArgValue :: (Typeable a) => ArgValue -> Maybe a
getArgValue (ArgValue _ v) = cast v


newtype OptionalValues = OptionalValues (Map String ArgValue)

instance Show OptionalValues where
    show (OptionalValues vMap) = "OptionalValues{" ++ vals ++ "}"
        where vals = intercalate "," . map show $  Map.elems vMap

get :: (Typeable v) => OptionalValues -> Optional vs v -> Maybe v
(OptionalValues oMp) `get` opt = getArgValue =<< argName opt `Map.lookup` oMp

flagSet :: OptionalValues -> Optional '[] Flag -> Bool
flagSet opts flag = isJust $ opts `get` flag

optValCount :: OptionalValues -> Int
optValCount (OptionalValues omap) = Map.size omap






