
{-# LANGUAGE DataKinds #-}

module Main where

--import Test.Hspec

import CArgs
import CArgs.Parsers
import CArgs.Descriptors


import Data.List (intercalate)

--main = hspec $ do
--    describe "" undefined

main = putStrLn $ intercalate "\n" testHelp'



args0 = CArgs{
    positionalArguments = Positional "datafile" text ["Path to the data file."]
                       :. Positional "epochs"   int  ["Epochs count"]
                       :. Positional "target"   text ["Target file"]
                       :. Nil
  , optionalArguments = [
        Opt' helpArg
      , Opt  lrateArg
      , Opt  hidlArg
      , Opt  extraArgs
    ]
}

helpArg  = optionalFlag "h" ["help"] ["Show help"]

lrateArg :: AnOptional Float
lrateArg = optional "L" ["learn-rate"] ["Specify learning rate"]
                    ["learning rate value, in [0, 1]"]

hidlArg :: AnOptional [Text]
hidlArg  = optional "H" ["hidden-layer"] ["Specify hidden layers"]
                    ["number of neurons, separated by ',' or ';'"]


extraArgs :: AnOptional (VarArg Text)
extraArgs = variable "E" ["extra"] ["Pass extra arguments"]
                     ["Extra arguments"]


testHelp  = fullHelp "exec_name" args0
testHelp' = helpFor args0 "learn-rate"

test = parseArgs args0 [
    "/home/user/data/file.dat", "500", "results.txt", "-h", "-L", "0.2"
  , "-E", "QWE", "RTY", "-H", "a,t"
  ]



