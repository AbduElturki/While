module ParsePrec where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as Lexer

type Int = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

data Aexp = N ParsePrec.Int
          | V Var
          | Mult Aexp Aexp
          | Add Aexp Aexp
          | Sub Aexp Aexp
            deriving (Show)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Le Aexp Aexp
          | Eq Aexp Aexp
            deriving (Show)

data Stm = Skip
         | Ass Var Aexp
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         | Block DecV DecP Stm
         | Call Pname
           deriving (Show)


whitespace :: Parser ()
whitespace = Lexer.space (void spaceChar) lineComment blockComment where
  lineComment  = Lexer.skipLineComment "//"         --Line comment
  blockComment = Lexer.skipBlockComment "/*" "*/"   --Block comment

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

symbol :: String -> Parser String
symbol = Lexer.symbol whitespace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

semi :: Parser String
semi = symbol ";"

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> whitespace

reservedwords :: [String] -- list of reserved words
reservedwords = [ "if"
                , "then"
                , "else"
                , "while"
                , "do"
                , "skip"
                , "true"
                , "false"
                , "proc"
                , "is"
                , "begin"
                , "end"
                , "call"
                , "var"
                ]

-- identifier and integer

identifier :: Parser String
identifier = (lexeme . try) (p >>= check) where
  p        = (:) <$> letterChar <*> many alphaNumChar
  check x  = if x `elem` reservedwords then fail $ "keyword " ++ show x ++ " cannot be an identifier" else return x

integer :: Parser Integer
integer = lexeme Lexer.integer

-- Main Parser

whileParser :: Parser Stm
whileParser = between whitespace eof statement

-- statement compostion

statement :: Parser Stm
statement = parens statement
         <|> sequenceOfStm

-- sequence of statement Parser

sequenceOfStm = do
  list <- (sepBy1 statement' semi)
  return $ if length list == 1 then head list else foldr1 Comp list

-- Single statement Parser

statement' :: Parser Stm
statement' =  ifStm         --If statement
          <|> whileStm      --While loop
          <|> skipStm
          <|> blockStm      --Skip
          <|> assignStm     --assignment
          <|> callprocStm

ifStm :: Parser Stm
ifStm = do
  reserved "if"
  cond <- bexp
  reserved "then"
  stm1 <- statement
  reserved "else"
  stm2 <- statement
  return $ If cond stm1 stm2

whileStm :: Parser Stm
whileStm = do
  reserved "while"
  cond <- bexp
  reserved "do"
  stm  <- statement
  return $ While cond stm

assignStm :: Parser Stm
assignStm = do
  var  <- identifier
  symbol ":="
  expr <- aexp
  return $ Ass var expr

skipStm :: Parser Stm
skipStm = reserved "skip" >> return Skip

blockStm :: Parser Stm
blockStm = do
  reserved "begin"
  dv  <- many decvclause
  dp  <- many decpclause
  stm <- statement
  reserved "end"
  return $ Block dv dp stm

callprocStm :: Parser Stm
callprocStm = do
  reserved "call"
  procp <- identifier
  return $ Call procp

-- Aexp and Bexp Parser

aexp :: Parser Aexp
aexp = makeExprParser term ops where
  ops = [ [InfixN (Mult  <$ symbol "*") ]
        , [InfixL (Add   <$ symbol "+") ,
           InfixR (Sub   <$ symbol "-") ]
        ]
  term =  parens aexp
      <|> V <$> identifier
      <|> N <$> integer

bexp :: Parser Bexp
bexp = makeExprParser term ops where
  ops = [ [Prefix  (Neg  <$ symbol "!") ]
        , [InfixN  (And  <$ symbol "&") ]
        ]
  term =  parens bexp
      <|> (TRUE  <$ reserved "true" )
      <|> (FALSE <$ reserved "false")
      <|> rexp

-- Relation expression

rexp = do
  a1 <- aexp
  op <-  (Le <$ symbol "<")
     <|> (Eq <$ symbol "=")
  a2 <- aexp
  return $ op a1 a2

-- DecV abd DecP clauses

tok :: String -> Parser String
tok t = try (string t <* whitespace)

decvclause = tok "var" *> ((,) <$> identifier) <* tok ":=" <*> aexp <* tok ";"

decpclause = tok "proc" *> ((,) <$> identifier) <* tok "is" <*> statement' <* tok ";"



parseString :: String -> IO()
parseString str = parseTest whileParser str
