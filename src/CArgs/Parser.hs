-----------------------------------------------------------------------------
--
-- Module      :  CArgs.Parser
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

{-# LANGUAGE DataKinds
           , KindSignatures
           , FlexibleInstances
           , ExistentialQuantification
           , FunctionalDependencies
           , TypeFamilies
       #-}

module CArgs.Parser where

-----------------------------------------------------------------------------


type Try  a = Either [String] a
type TryA a = Either (String -> [String]) a

-----------------------------------------------------------------------------



class ArgValParser p v where parseArgValue :: p v -> [String] -> (TryA v, [String])
                             parseArgType  :: p v -> String

data AnArgValParser v = forall p . (ArgValParser p v) => ArgValParser (p v)


-----------------------------------------------------------------------------


data SingleParser v = SingleParser String (String -> Maybe v)

parseSingle (SingleParser _ p) = p


-----------------------------------------------------------------------------

data GenericParser v = GenericParser String ([String] -> (TryA v, [String]))

instance Show (GenericParser v) where show (GenericParser name _) = name

instance ArgValParser GenericParser v where
    parseArgValue (GenericParser _ p) = p
    parseArgType  (GenericParser t _) = t

-----------------------------------------------------------------------------

instance Show (AnArgValParser a) where show (ArgValParser p) = parseArgType p
instance Show (SingleParser a) where show (SingleParser name _) = name

instance ArgValParser AnArgValParser v where
    parseArgValue (ArgValParser p) = parseArgValue p
    parseArgType  (ArgValParser p) = parseArgType p


instance ArgValParser SingleParser v where
    parseArgValue _ [] = (Left $ \a -> ["Argument '" ++ a ++ "' not provided"], [])
    parseArgValue (SingleParser _ p) (h:t) =
        case p h of Just v -> (Right v, t)
                    _      -> (Left $ msg h, h:t)
        where msg s arg = ["Failed to parse argument '" ++ arg
                        ++ "': " ++ s]
    parseArgType  (SingleParser t _) = t

-----------------------------------------------------------------------------


class CombinedArgValParser (subs :: [*] -> *) p vs v | p -> subs
    where
          parseArgCombined :: p vs v
                           -> subs vs
                           -> [String]
                           -> (TryA v, [String])
          combinedParserName :: p vs v -> String

-----------------------------------------------------------------------------

data ACombinedArgValParser subs vs v =
    forall p . (CombinedArgValParser subs p vs v) => CombinedArgValParser (p vs v)



instance Show (ACombinedArgValParser subs vs v) where
    show (CombinedArgValParser p) = combinedParserName p

instance CombinedArgValParser subs (ACombinedArgValParser subs) vs v where
--    type SubArgs ACombinedArgValParser = SubArgs
    parseArgCombined (CombinedArgValParser p) = parseArgCombined p
    combinedParserName (CombinedArgValParser p) = combinedParserName p

-----------------------------------------------------------------------------

data CombinedArgValParserStub (subs :: [*] -> *)  (vs :: [*]) v =
    CombinedArgValParserSingle (SingleParser v)
  | CombinedArgValParserVar    (AnArgValParser v)


instance CombinedArgValParser subs (CombinedArgValParserStub subs) '[v] v where
    parseArgCombined (CombinedArgValParserSingle p) _ l = parseArgValue p l
    combinedParserName (CombinedArgValParserSingle p) = parseArgType p



-----------------------------------------------------------------------------

data Combine2 a b = forall r . Combine2 (a -> b -> r)

data (vs ~ [a,b]) =>
    CombinedArgValParser2 (subs :: [*] -> *) a b vs v =
     CombinedArgValParser2 String
                           (subs [a,b] -> (SingleParser a, SingleParser b))
                           (a -> b -> v) -- (SingleParser a) (SingleParser b) (a -> b -> v)



instance CombinedArgValParser subs (CombinedArgValParser2 subs a b) [a,b] v where
    parseArgCombined (CombinedArgValParser2 _ splitSubs combine) ps l = r
        where (pa, pb) = splitSubs ps
              (ta, l')  = parseArgValue pa l
              (tb, l'') = parseArgValue pb l'
              r = case (ta,tb) of (Right a, Right b) -> (Right $ a `combine` b, l'')
                                  ((Left fail), _)   -> (Left fail, l)
                                  (_, (Left fail))   -> (Left fail, l)

    combinedParserName (CombinedArgValParser2 name _ _) = name

-----------------------------------------------------------------------------

data Combine3 a b c = forall r . Combine3 (a -> b -> c -> r)

data (vs ~ [a,b,c]) =>
    CombinedArgValParser3 (subs :: [*] -> *) a b c vs v =
     CombinedArgValParser3 String
                           (subs [a,b,c] -> (SingleParser a, SingleParser b, SingleParser c))
                           (a -> b -> c -> v)



instance CombinedArgValParser subs (CombinedArgValParser3 subs a b c) [a,b,c] v where
    parseArgCombined (CombinedArgValParser3 _ splitSubs combine) ps l = r
        where (pa, pb, pc) = splitSubs ps
              (ta, l')   = parseArgValue pa l
              (tb, l'')  = parseArgValue pb l'
              (tc, l''') = parseArgValue pc l''
              r = case (ta,tb,tc) of (Right a, Right b, Right c) -> (Right $ combine a b c, l''')
                                     ((Left fail), _, _)   -> (Left fail, l)
                                     (_, (Left fail), _)   -> (Left fail, l)
                                     (_, _, (Left fail))   -> (Left fail, l)

    combinedParserName (CombinedArgValParser3 name _ _) = name

-----------------------------------------------------------------------------

data ArgValParserCombination v =
     ArgValParserCombination String ([String] -> (TryA v, [String]))

instance ArgValParser ArgValParserCombination v where
    parseArgValue (ArgValParserCombination _ p) = p
    parseArgType  (ArgValParserCombination t _) = t



-----------------------------------------------------------------------------






