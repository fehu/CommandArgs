
{-# LANGUAGE DataKinds, GADTs #-}

module Main where

--import Test.Hspec

import CArgs

--main = hspec $ do
--    describe "" undefined


-----------------------------------------------------------------------------

args0 = CArgs{
    positionalArguments = Positional "datafile" text ["Path to the data file."]
                       :. Positional "epochs"   int  ["Epochs count"]
                       :. Positional "target"   text ["Target file"]
                       :. Nil
  , optionalArguments = [
        Opt helpArg
      , Opt verbArg
      , Opt lrateArg
      , Opt hidlArg
      , Opt extraArgs
    ]
}

lrateArg :: Optional1 Float
lrateArg = optional "L" ["learn-rate"] ["Specify learning rate"]
                    ["learning rate value, in [0, 1]"]

hidlArg :: Optional1 [Text]
hidlArg  = optional "H" ["hidden-layer"] ["Specify hidden layers"]
                    ["number of neurons, separated by ',' or ';'"]

extraArgs :: OptionalVar Text
extraArgs = variable "E" ["extra"] ["Pass extra arguments"]
                     "extra" ["Extra arguments"]


--testHelp  = fullHelp "exec_name" args0
--testHelp' = helpFor args0 "aaa"

test0 = parseArgs args0 [
    "/home/user/data/file.dat", "500", "results.txt", "-L", "0.2"
  , "-E", "QWE", "RTY", "--hidden-layer", "a,t", "-h", "A"
  ]

test0' = parseArgs args0 ["A", "--help", "hidden-layer"]


-----------------------------------------------------------------------------

args1 = CArgs{
    positionalArguments = Positional "x" float ["First number"]
                       :. Positional "y" float ["Second number"]
                       :. Nil
  , optionalArguments = [
        Opt optPlus
      , Opt optMult
      , Opt helpArg
      , Opt verbArg
    ]
}

optPlus = optionalFlag "" ["plus"] ["Sum two numbers (default)"]
optMult = optionalFlag "x" ["mult"] ["Multiply two numbers"]


main = application args1 argsHandler{
    handleMain = \(x :. y :. Nil) opts verb -> let f = maybe (+) (const (*)) (opts `get` optMult)
                                               in print $ f (posValue x) (posValue y)
  , handleOpts = handleHelp "test1" args1
}






