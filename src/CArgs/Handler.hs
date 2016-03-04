-----------------------------------------------------------------------------
--
-- Module      :  CArgs.Handler
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- | Handling arguments
--

{-# LANGUAGE TypeOperators, GADTs #-}

module CArgs.Handler (

-- * Handling arguments

  ArgsHandler(..)
, argsHandler

, handleArgs

-- * Predefined handlers

, handleHelp
, fullHelp
, helpFor

) where

import AList
import CArgs.Descriptors
import CArgs.Verbosity
import CArgs.Optionals
import CArgs.Parsers (text2str)
import CArgs.Values

import Data.Maybe (fromMaybe)
import Data.List (find, intercalate)

-----------------------------------------------------------------------------

newtype AlternativeHandler lp = AlternativeHandler
    (AList (Positional :-: Identity) lp -> OptionalValues -> Verbosity -> IO ())

-- | Arguments handling descriptor.
data ArgsHandler lps = ArgsHandler{
    -- | Case all positional arguments are in place.
    handleMain :: AAList AlternativeHandler lps
    -- | Case failed to parse positional arguments.
  , handleOpts :: OptionalValues -> Verbosity -> IO ()
    -- | Default verbosity level.
  , defaultVerbosity :: Verbosity
    -- | Set verbosity level from optional args.
  , updateVerbosity  :: OptionalValues -> Maybe Verbosity
}

-- | 'ArgsHandler' with predefined 'updateVerbosity' function and
--   'defaultVerbosity' = 'VerbErrors'.
argsHandler = ArgsHandler {
    defaultVerbosity = VerbErrors
  , updateVerbosity  = flip get verbArg
}

-- | Handles arguments, as described in given 'ArgsHandler'.
handleArgs :: ArgsHandler ls -> CArgValues ls -> IO ()
handleArgs ah av =
    do showProblems verb av
       case positionalValues av of Right alts -> f alts (handleMain ah)
--       sequence_ $ aa2List (f pos) (handleMain ah)
--       handleMain ah pos opts verb
                                   Left errs -> handleOpts ah opts verb
    where opts = optionalValues av
          verb = fromMaybe (defaultVerbosity ah) (updateVerbosity ah opts)
          f :: AAList AlternativeValues ls -> AAList AlternativeHandler ls -> IO ()
          f (AlternativeValues _ vals :.: _) (AlternativeHandler g :.: _) = g vals opts verb
          f (AlternativeFailed _ :.: t)      (_ :.: t')                   = f t t'
          f Nil'                              _                           = error
                                                                             "failed to parse args"


-----------------------------------------------------------------------------
-- Argument process verbosity

showProblems VerbSilent av = return ()
showProblems VerbErrors av =
    case positionalValues av of Left [_] -> return ()
                                Left err -> showErr err
                                _        -> return ()
showProblems VerbWarn av =
    do showProblems VerbErrors av
       showWarn $ optionalErrors av

showProblems VerbFull av = print av


showProbl pref = putStrLn . intercalate "\n" . map (pref++)
showWarn = showProbl "[WARNING] "
showErr  = showProbl "[ERROR]"

-----------------------------------------------------------------------------
-- Handle help

-- | Handles optional argument 'helpArg'.
handleHelp :: String            -- ^ Executable name.
           -> Multiline         -- ^ Help header.
           -> CArgs lp          -- ^ Command arguments descriptor.
           -> OptionalValues    -- ^ Extracted optional values.
           -> Verbosity         -- ^ For compatibility, not used.
           -> IO ()
handleHelp execName hheader ca opts _ =
    case opts `get` helpArg
    of Just (VarArg []) -> putStrLn . intercalate "\n" $ fullHelp execName hheader ca
       Just (VarArg l)  -> mapM_ (putStrLn . ('\n':) . intercalate "\n" . helpFor ca . text2str) l
       Nothing          -> return ()


-- | Help entries for all arguments.
fullHelp :: String      -- ^ Executable name.
         -> Multiline   -- ^ Help header.
         -> CArgs lp    -- ^ Command arguments descriptor.
         -> Multiline
fullHelp executable hheader d =
    "":hheader
    ++ ['\n' : executable ++ " " ++ unwords posArgs ++ " " ++ unwords optArgs]
    ++ dPosArgs ++ dOptArgs
    where (posArgs', optArgs') = getCArgs d
          posArgs = map (abrace . argId) posArgs'
          optArgs = map (sqbrace . argId) optArgs'
          prepArgs pref args = pref : addIndent (replicate 2 ' ') (concatMap argHelp args)
          dPosArgs = prepArgs "\nPositional:" posArgs'
          dOptArgs = prepArgs "\nOptional:\n" optArgs'



-- | Help entries for given argument.
helpFor :: CArgs lp     -- ^ Command arguments descriptor.
        -> String       -- ^ Arguments to describe.
        -> Multiline
helpFor d name = maybe notFound argHelp mbArg
    where mbArg = find ((name ==) . argId) (getAllCArgs d)
          notFound = ["Unknown argument : '" ++ name ++ "'"]



addIndent i = map (i++)

abrace s = "<" ++ s ++ ">"
sqbrace s = "[" ++ s ++ "]"

-----------------------------------------------------------------------------


