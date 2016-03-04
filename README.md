# Command args parsing and description utility for Haskell

### Describe Arguments

```haskell
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

```

### Describe Program

```haskell

main = application args1 argsHandler{
    handleMain = \(x :. y :. Nil) opts verb -> let f = maybe (+) (const (*)) (opts `get` optMult)
                                               in print $ f (posValue x) (posValue y)
  , handleOpts = handleHelp "test1" hheader args1
}

hheader = ["Apply a binary function to float numbers:"]

```

### Help Entries

```
user@domain > dist/build/tests/tests -h

Apply a binary function to float numbers:

test1 <x> <y> [plus] [mult] [help] [verbosity]

Positional:
  x  :: Float 	 First number
  y  :: Float 	 Second number

Optional:

  plus
     --plus
     Sum two numbers (default)

  mult
     -x --mult
     Multiply two numbers

  help <cmd...>
     -h --help
     Show help
        cmd...  :: Text 	 Commands to show the help for

  verbosity <value>
     -V --verbosity
     Set verbosity
        value :: Verbosity
          verbosity level: 0-3
          or 'silent', 'errors', 'warn', 'full'


```
