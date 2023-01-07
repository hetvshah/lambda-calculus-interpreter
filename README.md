# Lambda Calculus Interpreter
Parses well-formed lambda calculus expressions and assignments from the terminal or files and can perform the following:
- alpha conversion/substitution
- beta reduction
- eta reduction
- beta-eta reduction

The evaluation strategy (call by need or call by name) can be specified! 

Built with [Haskell](https://www.haskell.org/). Final project for CIS 5520.

## Authors
Yumika Amemiya (amemiyay) and Hetvi Shah (hetvis)

## Main components

### `Main.hs`
This file contains the main loop that interacts with the user. It holds and updates the user's current settings (store, file, reduction type, and evaluation type). 

The user can input the following commands: 
- `:l file`: loads LC expressions from the file and runs each line based on previously set reduction/evaluation type
- `:rt reductionType`: changes the type of reduction (`beta`, `eta`, `beta-eta`) so that all reductions following are reduced in that way. 
- `:et evaluationType`: changes the type of evaluation (`name`, `need`) so that all evaluations following it are evaluated in that way.
- `expression`: evaluates the expression with the previously set reduction/evaluation type
  
The default `reductionType` is beta, and the default `evaluationType` is name.
### `LCSyntax.hs`
This module defines data structures to represent the syntax of our variation of Lambda Calculus that supports definitions of variables, functions, applications, and primitive types (integers and booleans) and their basic unary and binary operations. 

This module contains:

- The definitions of the datatypes that represent the abstract syntax of Lambda Calculus expressions
- A pretty-printer that displays these datatypes as concise text
- Arbitrary instances for these datatypes that can be used for property based testing

### `LCParser.hs`
This module implements the parser for our variation of Lambda Calculus. 
It also uses definitions from the Parsers module from lecture, gathered together in the file `Parser.hs`. 

### `LCEvaluator.hs`

This module implements the evaluator for our variation of Lambda Calculus. 

Our evaluator supports:
- beta, eta, and beta-eta reduction
- call-by-name and call-by-need evaluation 
- evaluation of expressions and assignment of expressions to variables

## Tests 
Unit and quickcheck tests for `LCParser.hs` is contained in `LCParserTest.hs`. To run its tests:
```
stack ghci test/LCParserTest.hs
main
```

Unit and quickcheck tests for `LCEvaluator.hs` is contained in `LCEvaluatorTest.hs`. To run its tests:
```
stack ghci test/LCEvaluatorTest.hs
main
```

## Instructions for compilation

This project compiles with `stack build`. 

To run the main executable:
```
stack ghci app/Main.hs
main
```

Finally, you can start a REPL with `stack ghci`.
