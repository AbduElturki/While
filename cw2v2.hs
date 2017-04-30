module ParsePrec where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as Lexer

type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]
type Loc = Integer

data Aexp = N Num
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

parse :: String -> Stm
parse str = case parseMaybe (between whitespace eof statement) str of
                  Just a -> a
                  Nothing -> error "you suck"

--------------------------------------------------------------------------------

type Z = Num
type T = Bool
type State = Var -> Z
type S_envp = Pname -> Stm

data Config = Inter Stm State
            | Final State


evalA :: State -> Aexp -> Z
evalA st (N n)    = n
evalA st (V v)    = st v
evalA st (Add a b)  = (evalA st a) + (evalA st b)
evalA st (Sub a b)  = (evalA st a) - (evalA st b)
evalA st (Mult a b) = (evalA st a) * (evalA st b)

evalB :: State -> Bexp -> T
evalB st TRUE      = True
evalB st FALSE     = False
evalB st (Neg a)   = not (evalB st a)
evalB st (Eq a b)  = (evalA st a) == (evalA st b)
evalB st (Le a b)  = (evalA st a) <= (evalA st b)
evalB st (And a b) = (evalB st a) && (evalB st b)

update :: State -> Z -> Var -> State
update s v x y = if x == y then v else s y

cond :: (a->T, a->a, a->a) -> (a->a)
cond (p, g1, g2) s | p s = g1 s
                   | otherwise = g2 s

fix :: ((State->State)->(State->State))->(State->State)
fix ff = ff (fix ff)

{-
s_ds :: Stm -> State -> State
s_ds (Ass x a) s = update s (evalA s a) x
s_ds (Skip) s = s
s_ds (Comp ss1 ss2) s = ((s_ds ss2).(s_ds ss1)) s
s_ds (If b ss1 ss2) s = (cond (evalB s b, s_ds ss1, s_ds ss2)) s
s_ds (While b ss) s = (fix ff) s where
  ff :: (State->State) -> (State->State)
  ff g = cond (evalB b, g.s_ds ss, id)
-}

fv_aexp :: Aexp -> [Var]
fv_aexp (N n) = []
fv_aexp (V x) = [x]
fv_aexp (Add a1 a2) = (fv_aexp a1) +++ (fv_aexp a2)
fv_aexp (Mult a1 a2) = (fv_aexp a1) +++ (fv_aexp a2)
fv_aexp (Sub a1 a2) = (fv_aexp a1) +++ (fv_aexp a2)

-- concatenate (removing duplicates from first list)
(+++) :: Eq a => [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = if (elem x ys) then xs +++ ys else xs +++ (x:ys)

subst_aexp :: Aexp -> Var -> Aexp -> Aexp
subst_aexp (N n) v a = (N n)
subst_aexp (V x) v a = if (x == v) then a else (V x)
subst_aexp (Add a1 a2) v a = (Add (subst_aexp a1 v a) (subst_aexp a2 v a))
subst_aexp (Mult a1 a2) v a = (Mult (subst_aexp a1 v a) (subst_aexp a2 v a))
subst_aexp (Sub a1 a2) v a = (Sub (subst_aexp a1 v a) (subst_aexp a2 v a))

{-
ns_stm :: Config -> Config
ns_stm (Inter (Ass x a) s) = Final (update s (evalA a s) x)
ns_stm (Inter (Skip) s) = Final s
ns_stm (Inter (Comp ss1 ss2) s) = Final s'' where
  Final s'  = ns_stm (Inter ss1 s)
  Final s'' = ns_stm (Inter ss2 s')
ns_stm (Inter (If b ss1 ss2) s) | evalB b s = Final s'
                                | otherwise = Final s''
                                where
                                  Final s' = ns_stm (Inter ss1 s)
                                  Final s'' = ns_stm (Inter ss2 s)
ns_stm (Inter (While b ss) s) | evalB b s = Final s''
                              | otherwise = Final s
                              where
                                Final s' = ns_stm (Inter ss s)
                                Final s'' = ns_stm (Inter (While b ss) s')


s_ns :: Stm -> State -> State
s_ns ss s = s' where
  Final s' = ns_stm (Inter ss s)
-}

decvupdate :: State
decvupdate = undefined


s_dynamic :: Stm -> State -> State
s_dynamic ss s = s' where
  Final s' = ds_stm (const undefined) (Inter ss s)

ds_stm :: S_envp -> Config  -> Config
ds_stm envp (Inter (Ass x a) s) = Final (update s (evalA s a) x)
ds_stm envp (Inter Skip s) = Final s
ds_stm envp (Inter (Comp stm1 stm2) s)= Final s''
    where
      Final s'  = ds_stm envp (Inter stm1 s)
      Final s'' = ds_stm envp (Inter stm2 s')
ds_stm envp (Inter (If bexp stm1 stm2) s)
  | evalB s bexp = Final s'
  where
    Final s' = ds_stm envp (Inter stm1 s)
ds_stm envp (Inter (If bexp stm1 stm2) s)
  | not(evalB s bexp) = Final s'
  where
    Final s' = ds_stm envp (Inter stm2 s)
ds_stm envp (Inter (While bexp stm) s)
  | evalB s bexp = Final s''
  where
    Final s'  = ds_stm envp (Inter stm s)
    Final s'' = ds_stm envp (Inter (While bexp stm) s')
ds_stm envp (Inter (While bexp stm) s)
  | not(evalB s bexp ) = Final s
ds_stm envp (Inter (Block decv decp stm) s) = ds_stm (fold_procedures envp decp) (Inter stm s)
ds_stm envp (Inter (Call pname) s) = ds_stm envp (Final s)

fold_procedures :: S_envp -> DecP -> S_envp
fold_procedures envp decp = envp where
  envp = foldr upd_p envp decp

upd_p :: (Pname, Stm) -> S_envp -> S_envp
upd_p (pname, stmt) envp y
  | pname == y = stmt
  | otherwise  = envp y

init_s :: State
init_s a = 0
