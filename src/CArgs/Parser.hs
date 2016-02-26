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

instance Show (AnArgValParser a) where show (ArgValParser p) = parseArgType p
instance Show (SingleParser a) where show (SingleParser name _) = name

instance ArgValParser AnArgValParser v where
    parseArgValue (ArgValParser p) = parseArgValue p
    parseArgType  (ArgValParser p) = parseArgType p


instance ArgValParser SingleParser v where
    parseArgValue (SingleParser _ p) (h:t) =
        case p h of Just v -> (Right v, t)
                    _      -> (Left $ msg h, h:t)
        where msg s arg = ["Failed to parse argument '" ++ arg
                        ++ "', unexpected " ++ s]
    parseArgType  (SingleParser t _) = t

-----------------------------------------------------------------------------


class CombinedArgValParser (subs :: [*] -> *) p vs v | p -> subs
    where
          parseArgCombined :: p vs v
                           -> subs vs
                           -> [String]
                           -> (TryA v, [String])
          combinedParserName :: p vs v -> String

data ACombinedArgValParser subs vs v =
    forall p . (CombinedArgValParser subs p vs v) => CombinedArgValParser (p vs v)

data CombinedArgValParserStub (subs :: [*] -> *)  (vs :: [*]) v =
    CombinedArgValParserSingle (SingleParser v)
--  | CombinedArgValParserVar    ([String] -> (Maybe v, String))


-----------------------------------------------------------------------------


instance Show (ACombinedArgValParser subs vs v) where
    show (CombinedArgValParser p) = combinedParserName p

instance CombinedArgValParser subs (ACombinedArgValParser subs) vs v where
--    type SubArgs ACombinedArgValParser = SubArgs
    parseArgCombined (CombinedArgValParser p) = parseArgCombined p
    combinedParserName (CombinedArgValParser p) = combinedParserName p

instance CombinedArgValParser subs (CombinedArgValParserStub subs) '[v] v where
    parseArgCombined (CombinedArgValParserSingle p) _ l = parseArgValue p l
    combinedParserName (CombinedArgValParserSingle p) = parseArgType p



-----------------------------------------------------------------------------


data ArgValParserCombination v =
     ArgValParserCombination String ([String] -> (TryA v, [String]))

instance ArgValParser ArgValParserCombination v where
    parseArgValue (ArgValParserCombination _ p) = p
    parseArgType  (ArgValParserCombination t _) = t



-----------------------------------------------------------------------------






