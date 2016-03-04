-----------------------------------------------------------------------------
--
-- Module      :  AList
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE DataKinds
           , TypeOperators
           , GADTs
           , MultiParamTypeClasses
           , FlexibleInstances
           , Rank2Types
           , FlexibleContexts
           , TypeFamilies
           , UndecidableInstances
       #-}

module AList (

-- * Constructors

  AList((:.), Nil)

, AAList(..)

-- * Lists types combination.

, (::+)
, (:++:)

-- * Functions on 'AList' underlying types

, AnyFunc, DepFunc, DepFuncM

, aLength
, aNull

, a2List
, a2ListWithFilter
, aa2List

, aAnyOf
, MapAList(..)


-- * 'AList's of pairs

, (:-:)((:-:))
, (:<:)((:<:))

, Identity(..), unwrapId

, aZip, aUnzip

-- * Type dependent 'Either'

, EitherDR(LeftDR, RightDR)

, isLeftDR, isRightDR

, leftDRUnsafe, rightDRUnsafe

, leftDR, rightDR

-- * 'AList's of 'Either's

, condMap
, eitherDR


) where


import Data.Maybe (maybeToList)

-----------------------------------------------------------------------------

-- | A heterogenous list of type dependant values.
data AList (d :: * -> *) (l::[*]) where
    Nil :: AList c '[]
    (:.) :: (Show (d a)) => d a -> AList d l -> AList d (a ': l)

infixr :.

data AAList (d :: [*] -> *) (l::[[*]]) where
    Nil'  :: AAList c '[]
    (:.:) :: (Show (d a)) => d a -> AAList d l -> AAList d (a ': l)

instance Show (AList c l) where show (h:.Nil) = show h
                                show (h:.t)   = show h ++ " : " ++ show t
                                show Nil      = "[]"

instance Show (AAList c ls) where show (h:.:Nil') = show h
                                  show (h:.:t)    = show h ++ " :.: " ++ show t
                                  show Nil'       = "[]"

-- | Append type.
type family (::+) (l :: [*]) (x :: *) :: [*] where
    '[] ::+ x      = '[x]
    (h ': t) ::+ x = h ': (t ::+ x)

-- | Concat two type lists.
type family (:++:) (l :: [*]) (r :: [*]) :: [*] where
    l :++: '[]      = l
    l :++: (h ': t) = (l ::+ h) :++: t

-----------------------------------------------------------------------------

type AnyFunc a b = forall x . a x -> b
type AnyFunc' a b = forall (x :: [*]) . a x -> b
type DepFunc a b = forall x . a x -> b x
type DepFuncM m a b = forall x . a x -> m (b x)

-----------------------------------------------------------------------------

-- | Filter and transform 'AList' to list, given a 'AnyFunc' filter and
--   transformation functions for the underlying values.
a2ListWithFilter :: AnyFunc c Bool -> AnyFunc c r -> AList c l -> [r]
a2ListWithFilter f g (h:.t) = if f h then g h : a2ListWithFilter f g t
                                     else       a2ListWithFilter f g t
a2ListWithFilter _ _ Nil    = []

-- | Transform 'AList' to list, given a 'AnyFunc' transformation function
--   for the underlying values.
a2List :: AnyFunc c r -> AList c l -> [r]
a2List = a2ListWithFilter (const True)

-- | Transform 'AAList' to list, given a transformation function
--   for the underlying 'AList's.
aa2List :: AnyFunc' c r -> AAList c ls -> [r]
aa2List f (hl :.: tl) = f hl : aa2List f tl
aa2List _ _           = []


-- | Length of an 'AList'.
aLength :: AList c l -> Int
aLength (h:.t) = aLength t + 1
aLength Nil    = 0

aNull Nil = True
aNull _   = False

-- | Tests if any underlying value complies with predicate.
aAnyOf :: AnyFunc a Bool -> AList a l -> Bool
aAnyOf _ Nil = False
aAnyOf f (h:.t) | f h       = True
                | otherwise = f `aAnyOf` t


-- | Applies 'DepFunc' over underlying values.
class MapAList l r where aMap :: DepFunc c r -> AList c l -> AList r l

instance MapAList '[] r where aMap _ _ = Nil
instance (Show (r h), MapAList t r) => MapAList (h ': t) r
    where aMap f (h:.t) = f h :. aMap f t

-----------------------------------------------------------------------------

-- | A pair of dependent types.
data (:-:) a b t = (:-:) (a t) (b t)
-- | A pair with a dependent left type.
data (:<:) a b t = (:<:) (a t)  b

-- | Identity type.
newtype Identity a = Identity a deriving (Eq, Ord)
unwrapId (Identity a) = a


instance (Show (a t), Show (b t))   => Show ((a :-: b) t)
    where show (l :-: r) = show l ++ " ~ " ++ show r
instance (Show (a t), Show b)       => Show ((a :<: b) t)
    where show (l :<: r) = show l ++ " ~ " ++ show r
instance (Show a)                   => Show (Identity a)
    where show (Identity i) = show i


-- | Zips 'AList' with a list.
aZip :: (Show b) => AList c l -> [b] -> AList (c :<: b) l
aZip (h:.t) (h':t') = (h :<: h') :. aZip t t'
aZip Nil _ = Nil
aZip _  [] = error "list is shorter than AList"

-- | Unzip 'AList' of '(:-:)' pairs.
aUnzip :: (AUnzip' a b l '[] l) => AList (a :-: b) l -> (AList a l, AList b l)
aUnzip l = aUnzip' l (Nil, Nil)

class AUnzip' a b l r t where
    aUnzip' ::  AList (a :-: b) l
            -> (AList a r, AList b r)
            -> (AList a t, AList b t)

instance AUnzip' a b '[] r r where aUnzip' _ acc = acc

instance (AUnzip' a b t (h ': r) tt, ((h ': r)  :++: r) ~ tt, Show (a h), Show (b h)) =>
    AUnzip' a b (h ': t) r tt where
        aUnzip' ((a :-: b) :. t) (accA, accB) = aUnzip' t (a :. accA, b :. accB)

-----------------------------------------------------------------------------

-- | 'Either' with dependent right type.
data EitherDR l r x = LeftDR l
                    | RightDR (r x)
                    deriving Show

-- | Is left?
isLeftDR  x = case x of LeftDR _ -> True
                        _        -> False
-- | Is right?
isRightDR x = case x of RightDR _ -> True
                        _         -> False

-- | Get right value, causing an error if it's actually left.
rightDRUnsafe (RightDR r) = r
-- | Get left value, causing an error if it's actually right.
leftDRUnsafe  (LeftDR l)  = l

-- | Get right value as 'Maybe'.
rightDR x = case x of RightDR x -> Just x
                      _         -> Nothing

-- | Get left value as 'Maybe'.
leftDR x  = case x of LeftDR x -> Just x
                      _        -> Nothing

-----------------------------------------------------------------------------


-- | Applies main transformation to each value, until
--   'Nothing' is met, in which case applies the
--    alternative transformation to the rest.
condMap :: (MapAList l (EitherDR (Maybe r') r), MapAList l r) =>
           DepFuncM Maybe c r           -- ^ Underlying value optional transformation.
        -> AnyFunc c (Maybe r')         -- ^ Alternative (failure) transformation.
        -> AList c l
        -> Either [r'] (AList r l)

condMap fm fa l =
    if isLeftDR `aAnyOf` eithers
            then Left . concatMap maybeToList $
                        a2ListWithFilter isLeftDR leftDRUnsafe eithers
            else Right $ aMap rightDRUnsafe eithers
    where eithers = aMap (f fm fa) l
          f fm' fa' x = case fm' x of Just x' -> RightDR x'
                                      _       -> LeftDR $ fa' x




-- | Transforms an 'AList' of 'EitherDR' into an 'Either' of 'AList'.
eitherDR :: (MapAList l r, MapAList l (EitherDR (Maybe b) r)) =>
            AList (EitherDR b r) l -> Either [b] (AList r l)
eitherDR = condMap rightDR leftDR

-----------------------------------------------------------------------------




