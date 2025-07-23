
import L.L2.Interpreter.Interp
import L.L2.Frontend.Syntax
import Utils.Pretty

import System.Environment
import System.FilePath
import System.Process

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

-- running the compiler / interpreter

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] ->
    lexerOnly file
  [Parser file] ->
    parserOnly file
  [Interpret file] ->
    interpret file
  [VM file] ->
    v1Compiler file
  [C file] ->
    cCompiler file
  _ -> helpMessage


-- Implement the function to do lexical analysis for L2 programs and outputs the tokens

lexerOnly :: FilePath -> IO ()
lexerOnly file = do
  contents <- readFile file
  let tokens = alexLexer contents
  print tokens 


-- Implement the function to do syntax analysis for L2 programs and outputs the syntax tree

parserOnly :: FilePath -> IO ()
parserOnly file = do
  contents <- readFile file
  let tokens = alexLexer contents 
  case parse program "input" tokens of  
    Left err -> print err  
    Right ast -> print ast 


-- Implement the whole interpreter pipeline: lexical and syntax analysis and then interpret the program

interpret :: FilePath -> IO ()
interpret file = do
  contents <- readFile file
  let tokens = alexLexer contents  
  case parse program "input" tokens of 
    Left err -> print err  
    Right ast -> do
      let result = runInterpreter ast
      print result


-- Implement the whole compiler pipeline: lexical, syntax and semantic analysis and then generate v1 instructions from the program.

v1Compiler :: FilePath -> IO ()
v1Compiler file = do
  contents <- readFile file
  let tokens = alexLexer contents
  case parse program "input" tokens of 
    Left err -> print err
    Right ast -> do
      let v1Instructions = generateV1Code ast  
      putStrLn (pretty v1Instructions)  


-- Implement the whole executable compiler, using C source and GCC.

cCompiler :: FilePath -> IO ()
cCompiler file = do
  contents <- readFile file
  let tokens = alexLexer contents
  case parse program "input" tokens of  
    Left err -> print err
    Right ast -> do
      let cCode = generateCCode ast  
      writeFile "output.c" cCode  
      _ <- runCommand "gcc -o output output.c"  
      putStrLn "Executável gerado com sucesso!"


-- help message

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L2 language"
                       , "Usage: l2 [--lexer-only | --parse-only | --interpret | --help]"
                       , "--lexer-only: does the lexical analysis of the input program."
                       , "--parse-only: does the syntax analysis of the input program."
                       , "--interpret: does the syntax and semantic analysis and interpret the input program."
                       , "--v1: does the syntax and semantic analysis and then generates V1 code."
                       , "--c: does the syntax and semantic analysis, generates C code and uses GCC to generate an executable."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Interpret FilePath
  | VM FilePath
  | C FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--parse-only" : arg : _) -> [Parser arg]
    ("--interpret" : arg : _) -> [Interpret arg]
    ("--v1" : arg : _) -> [VM arg]
    ("--c" : arg : _) -> [C arg]
    _ -> [Help]
