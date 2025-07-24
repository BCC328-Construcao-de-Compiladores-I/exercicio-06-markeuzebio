import L.L2.Interpreter.Interp (evalL2)
import L.L2.Frontend.Syntax
import L.L2.Frontend.Lexer
import L.L2.Frontend.LALRParser (lalrParser)
import L.L2.Backend.V1Codegen
import Utils.Pretty

import System.FilePath
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO

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
  file_exists <- doesFileExist file
  if file_exists
    then do
      source_code <- readFile file
      putStrLn ((unlines . parseLexerOutput . lexer) source_code)
    else error "File passed by argument does not exist!"


-- Implement the function to do syntax analysis for L2 programs and outputs the syntax tree

parserOnly :: FilePath -> IO ()
parserOnly file = do
  file_exists <- doesFileExist file
  if file_exists
    then do
      source_code <- readFile file
      result <- lalrParser source_code
      case result of
        Left  err_message -> putStrLn err_message
        Right ast_tree    -> putStrLn (show ast_tree)
    else error "File passed by argument does not exist!"

-- Implement the whole interpreter pipeline: lexical and syntax analysis and then interpret the program

interpret :: FilePath -> IO ()
interpret file = do
  file_exists <- doesFileExist file
  if file_exists
    then do
      source_code <- readFile file
      parseResult <- lalrParser source_code
      case parseResult of
        Left  err_message -> putStrLn err_message
        Right ast_tree    -> do
          interpretResult <- evalL2 ast_tree
          case interpretResult of
            Left  err_message -> putStrLn err_message
            Right env         -> putStrLn (show env)
    else error "File passed by argument does not exist!"

-- Implement the whole compiler pipeline: lexical, syntax and semantic analysis and then generate v1 instructions from the program.

v1Compiler :: FilePath -> IO ()
v1Compiler file = do
  file_exists <- doesFileExist file
  if file_exists
    then do
      source_code <- readFile file
      result <- lalrParser source_code
      case result of
        Left  err_message -> putStrLn err_message
        Right ast_tree    -> do
          interpretResult <- evalL2 ast_tree
          case interpretResult of
            Left  err_message -> putStrLn err_message
            Right env         -> writeFile "ex1.v2" (show (ppr $ v2Codegen ast_tree))
    else error "File passed by argument does not exist!"

-- Implement the whole executable compiler, using C source and GCC.

cCompiler :: FilePath -> IO ()
cCompiler file = error "Not implemented!"

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

-- It pretties the stream of Tokens output to be printed on console
parseLexerOutput :: Either String [Token] -> [String]
parseLexerOutput (Left err)                                     = [err]
parseLexerOutput (Right [])                                     = []
parseLexerOutput (Right ((Token (line, col) (TNumber n)) : xs)) = ["Numero " ++ show n ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TString s)) : xs)) = ["String \"" ++ s ++ "\" Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TLParen)) : xs))   = ["Parentesis (" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TRParen)) : xs))   = ["Parentesis )" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TPlus)) : xs))     = ["Mais +" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TMinus)) : xs))    = ["Menos -" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TTimes)) : xs))    = ["Vezes *" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TId s)) : xs))     = ["Identificador " ++ s ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TAssign)) : xs))   = ["Atribuicao :=" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TRead)) : xs))     = ["Palavra reservada read" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TPrint)) : xs))    = ["Palavra reservada print" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TDef)) : xs))      = ["Palavra reservada def" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TIn)) : xs))       = ["Palavra reservada in" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TEnd)) : xs))      = ["Palavra reservada end" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TComma)) : xs))    = ["Virgula ," ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TEOC)) : xs))      = ["Ponto e virgula ;" ++ " Linha:" ++ show line ++ " Coluna:" ++ show col] ++ parseLexerOutput (Right xs)
parseLexerOutput (Right ((Token (line, col) (TEOF)) : xs))      = parseLexerOutput (Right xs)
