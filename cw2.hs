module ParsePrec where

import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

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

-- Language Definition

languageDef =
  emptyDef  { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "while"
                                      , "do"
                                      , "skip"
                                      , "true"
                                      , "false"
                                      ]
            , Token.reservedOpNames = [ "+", "-", "*", ":="
                                      , "<", "=", "&", "!"
                                      ]
}

-- Lexer

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer

--Main Parser

whileParser :: Parser Stm
whileParser = whiteSpace >> statement <* eof

statement :: Parser Stm
statement = parens statement
         <|> sequenceOfStm

sequenceOfStm = do
  list <- (sepBy1 statement' semi)
  return $ if length list == 1 then head list else foldr1 Comp list

statement' :: Parser Stm
statement' =  ifStm         --If statement
          <|> whileStm      --While loop
          <|> skipStm       --Skip
          <|> assignStm     --assignment

ifStm :: Parser Stm
ifStm = do
  reserved "if"
  cond  <- bexp
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
  stm <- statement
  return $ While cond stm

assignStm :: Parser Stm
assignStm = do
  var  <- identifier
  reservedOp ":="
  expr <- aexp
  return $ Ass var expr

skipStm :: Parser Stm
skipStm = reserved "skip" >> return Skip

-- Aexp Parser

aexp :: Parser Aexp
aexp = buildExpressionParser ops term where
  ops = [ [Infix (Mult  <$ reservedOp "*") AssocNone ]
        , [Infix (Add   <$ reservedOp "+") AssocLeft ,
           Infix (Sub   <$ reservedOp "-") AssocRight]
        ]
  term =  parens aexp
       <|> liftM V identifier
       <|> liftM N integer

-- Bexp Parser

bexp :: Parser Bexp
bexp = buildExpressionParser ops term where
  ops = [ [Prefix (Neg  <$ reservedOp "!")          ]
        , [Infix  (And  <$ reservedOp "&") AssocNone]
        ]
  term =  parens bexp
      <|> (TRUE  <$ reserved "true" )
      <|> (FALSE <$ reserved "false")
      <|> rexp

-- Relation expression

rexp =
  do a1 <- aexp
     op <-  (Le <$ reservedOp "<")
        <|> (Eq <$ reservedOp "=")
     a2 <- aexp
     return $ op a1 a2

-- Parses String inputs

parseString :: String -> Stm
parseString str =
  case parse whileParser "" str of
    Left  e -> error $ show e
    Right r -> r

-- Parses Files

parseFile :: String -> IO Stm
parseFile file = do
  program  <- readFile file
  case parse whileParser "" program of
    Left  e -> print  e >> fail "parse error"
    Right r -> return r
