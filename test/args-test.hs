
{-# LANGUAGE DataKinds #-}

module Main where

--import Test.Hspec

import CArgs
import CArgs.Parsers


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



make :: (DefaultSingleParser v) => ACombinedArgValParser (AList Positional) '[v] v
make = CombinedArgValParser $ CombinedArgValParserSingle singleParser

make' = CombinedArgValParser . CombinedArgValParserSingle

optionalFlag shorts longs descr = Optional shorts longs (make' flag) descr Nil

optional :: (DefaultSingleParser v) =>
    [Char] -> [String] -> Multiline -> Multiline -> AnOptional v
optional shorts longs descr argDescr = AnOptional $
    Optional shorts longs make descr (auto argDescr)

auto :: (DefaultSingleParser t) => Multiline -> AList Positional '[t]
auto descr = Positional "value" singleParser descr :. Nil



test = parsePositional (positionalArguments args0) [
    "/home/user/data/file.dat", "500", "results.txt"
  ]






