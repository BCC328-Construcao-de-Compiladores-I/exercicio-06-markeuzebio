{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- module L.L1.Frontend.Lexer (Token (..), Lexeme (..), lexer, Alex, alexMonadScan) where
module L.L2.Frontend.Lexer where

import Control.Monad
}

%wrapper "monadUserState"

$digit    = 0-9              -- digits
$id_char  = [a-zA-Z_]        -- id characters

-- second RE macros

@number     = $digit+
@string     = \"([^\"\\]|\\.)*\"
@identifier = $id_char+[$id_char$digit]*

-- tokens declarations

tokens :-
      -- whitespace and line comments
      <0> $white+          ;
      <0> "//" .*          ;
      -- multi-line comments
      <0> "\*"             { nestComment `andBegin` state_comment }
      <0> "*/"             { \ _ _ -> alexError "Error! Unexpected close comment!" }
      <state_comment> "\*" { nestComment }
      <state_comment> "*/" { unnestComment }
      <state_comment> .    ;
      <state_comment> "\n"  ;
      -- left tokens
      <0> @number          {mkNumber}
      <0> @string          {mkString}
      <0> "("              {simpleToken TLParen}
      <0> ")"              {simpleToken TRParen}
      <0> "+"              {simpleToken TPlus}
      <0> "-"              {simpleToken TMinus}
      <0> "*"              {simpleToken TTimes}
      <0> "/"              {simpleToken TDiv}
      <0> ":="             {simpleToken TAssign}
      <0> ";"              {simpleToken TEOC}
      <0> ","              {simpleToken TComma}
      <0> "read"           {simpleToken TRead}
      <0> "print"          {simpleToken TPrint}
      <0> "def"            {simpleToken TDef}
      <0> "in"             {simpleToken TIn}
      <0> "end"            {simpleToken TEnd}
      <0> @identifier      {mkIdentifier}
{

-- user state

data AlexUserState = AlexUserState {
  -- comment nesting level
  nestLevel :: Int 
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right(s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f
  = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

-- Aux function to get alex pos

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

-- definition of the EOF token

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == state_comment) $
    alexError "Error: unclosed comment"
  pure $ Token (position pos) TEOF

-- dealing with comments

nestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction Token
unnestComment input len
  = do
      s <- get
      let level = (nestLevel s) - 1
      put s{nestLevel = level}
      when (level == 0) $
        alexSetStartCode 0
      skip input len

-- token definition

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
  = TNumber Int
  | TString String
  | TLParen
  | TRParen
  | TPlus
  | TMinus
  | TTimes
  | TDiv
  | TId String
  | TAssign
  | TRead
  | TPrint
  | TDef
  | TIn
  | TEnd
  | TComma
  | TEOC
  | TEOF
  deriving (Eq, Ord, Show)

mkNumber :: AlexAction Token
mkNumber (posn, _, _, str) len
  = pure $ Token (position posn) (TNumber $ read $ take len str)

mkString :: AlexAction Token
mkString (posn, _, _, str) len = return $ Token (position posn) (TString $ parseString $ take len str)
  where
    parseString s         = (interpretScapes . removeQuotes) s
    removeQuotes ('"':xs) = (reverse . dropWhile (== '"') . reverse) xs
    removeQuotes s        = s
    interpretScapes [] = []
    interpretScapes ('\\':'n':xs)  = '\n' : interpretScapes xs
    interpretScapes ('\\':'t':xs)  = '\t' : interpretScapes xs
    interpretScapes ('\\':'\\':xs) = '\\' : interpretScapes xs
    interpretScapes ('\\':'"':xs)  = '\"' : interpretScapes xs
    interpretScapes ('\\':x:xs)    = x : interpretScapes xs
    interpretScapes (x:xs)         = x : interpretScapes xs

simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (posn, _, _, str) _ = return $ Token (position posn) lx

mkIdentifier :: AlexAction Token
mkIdentifier (posn, _, _, str) len = return $ Token (position posn) (TId $ take len str)

lexer :: String -> Either String [Token]
lexer s = runAlex s go
  where
    go = do
      output <- alexMonadScan
      if lexeme output == TEOF then
        pure [output]
      else (output :) <$> go
}
