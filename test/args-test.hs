
{-# LANGUAGE DataKinds #-}

module Main where

--import Test.Hspec

import CArgs
import CArgs.Parsers
import CArgs.Descriptors

--import CArgs.Parsers.Internal


--main = hspec $ do
--    describe "" undefined

main = print test



args0 = CArgs{
    positionalArguments = Positional "datafile" text ["Path to the data file."]
                       :. Positional "epochs"   int ["Epochs count"]
                       :. Positional "target"   text ["Target file"]
                       :. Nil
  , optionalArguments = [
        Opt' helpArg
      , Opt  lrateArg
      , Opt  hidlArg
    ]
}

helpArg  = optionalFlag "h" ["help"] ["Show help"]

lrateArg :: AnOptional Float
lrateArg = optional "L" ["learn-rate"] ["Specify learning rate"]
                    ["learning rate value, in [0, 1]"]

hidlArg :: AnOptional [Text]
hidlArg  = optional "H" ["hidden-layer"] ["Specify hidden layers"]
                    ["number of neurons, separated by ',' or ';'"]





test = parseArgs args0 [
    "/home/user/data/file.dat", "500", "results.txt", "-h", "-L", "0.2", "-x"
  ]

--test' = parseOptionals (optionalArguments args0) ["-h", "-L", "0.2"]




