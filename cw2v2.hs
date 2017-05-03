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
          <|> blockStm
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
  op <-  (Le <$ symbol "<=")
     <|> (Eq <$ symbol "=")
  a2 <- aexp
  return $ op a1 a2

-- DecV abd DecP clauses

tok :: String -> Parser String
tok t = try (string t <* whitespace)

decvclause = tok "var" *> ((,) <$> identifier) <* tok ":=" <*> aexp <* tok ";"

decpclause = tok "proc" *> ((,) <$> identifier) <* tok "is" <*> statement' <* tok ";"

parse :: String -> Stm
parse str = case parseMaybe whileParser str of
  Just a -> a
  Nothing -> error "you suck fam. git gud"

--------------------------------------------------------------------------------

type Z = Num
type T = Bool
type State = Var -> Z
type D_envp = Pname -> Stm
newtype M_envp = M_envp ( Pname -> (Stm,M_envp))
--type S_envp = Pname -> Stm -> Envv -> Envp
--type Envv = Var -> Lock
--type Loc = Z

--new:: Loc -> Loc
--new x = x + 1

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

-- Dynamic Scoping

s_dynamic :: Stm -> State -> State
s_dynamic stm s = s' where
  Final s' = ds_stm (const undefined) (Inter stm s)

ds_stm :: D_envp -> Config  -> Config
ds_stm envp (Inter (Ass x a) s) = Final (update s (evalA s a) x)
ds_stm envp (Inter Skip s) = Final s
ds_stm envp (Inter (Comp stm1 stm2) s)= Final s''
    where
      Final s'  = ds_stm envp (Inter stm1 s)
      Final s'' = ds_stm envp (Inter stm2 s')
ds_stm envp (Inter (If b stm1 stm2) s)
  | evalB s b = Final s'
  | otherwise = Final s'' where
    Final s'  = ds_stm envp (Inter stm1 s)
    Final s'' = ds_stm envp (Inter stm2 s)
ds_stm envp (Inter (While b ss) s)
  | evalB s b = Final s''
  | otherwise = Final s where
    Final s' = ds_stm envp (Inter ss s)
    Final s'' = ds_stm envp (Inter (While b ss) s')
ds_stm envp (Inter (Block decv decp stm) s) = Final s''' where
  s' = updateDecV s decv
  envp' = updateDecP envp decp
  Final s'' = ds_stm envp' (Inter stm s')
  s''' = (\var -> if (var `elem` (map fst decv)) then s var else s'' var)
ds_stm envp (Inter (Call pname) s) =  ds_stm envp (Inter (envp pname) s)

updateDecV :: State-> DecV -> State
updateDecV s decV = foldl updateDecV' s decV
  where
    updateDecV':: State -> (Var, Aexp) -> State
    updateDecV' s (var, aexp) = \var' -> case () of
            _ | var' == var -> evalA s aexp
              | otherwise   -> s var'

updateDecP :: D_envp -> DecP -> D_envp
updateDecP envp decP = foldl updateDecP' envp decP
  where
    updateDecP':: D_envp -> (Pname, Stm) -> D_envp
    updateDecP' envp (pname, stmt) = \pname' -> case () of
            _ | pname' == pname -> stmt
              | otherwise   -> envp pname'


-- Mixed Scoping

s_Mixed :: Stm -> State -> State
s_Mixed stm s = s' where
  Final s' = ms_stm (M_envp (const undefined)) (Inter stm s)

ms_stm :: M_envp -> Config  -> Config
ms_stm envp (Inter (Ass x a) s) = Final (update s (evalA s a) x)
ms_stm envp (Inter Skip s) = Final s
ms_stm envp (Inter (Comp stm1 stm2) s)= Final s''
    where
      Final s'  = ms_stm envp (Inter stm1 s)
      Final s'' = ms_stm envp (Inter stm2 s')
ms_stm envp (Inter (If b stm1 stm2) s)
  | evalB s b = Final s'
  | otherwise = Final s'' where
    Final s'  = ms_stm envp (Inter stm1 s)
    Final s'' = ms_stm envp (Inter stm2 s)
ms_stm envp (Inter (While b ss) s)
  | evalB s b = Final s''
  | otherwise = Final s where
    Final s' = ms_stm envp (Inter ss s)
    Final s'' = ms_stm envp (Inter (While b ss) s')
ms_stm envp (Inter (Block decv decp stm) s) = Final s''' where
  s' = updateDecV s decv
  envp' = updateDecPMixed envp decp
  Final s'' = ms_stm envp' (Inter stm s')
  s''' = (\var -> if (var `elem` (map fst decv)) then s var else s'' var)
ms_stm envp (Inter (Call pname) s) = Final s' where
  envp  =  updateDecPMixed' envp (pname, Call pname)
  Final s' =  ms_stm envp (Inter (envp pname) s) ----------------- might loop

updateDecPMixed ::  M_envp -> DecP -> M_envp
updateDecPMixed envp decP = foldl updateDecPMixed' envp decP

updateDecPMixed':: M_envp -> (Pname, Stm) -> M_envp
updateDecPMixed' (M_envp envp) (pname, stmt) = M_envp (\pname' ->
  case pname' == pname of
    True  -> (stmt, M_envp envp)
    otherwise -> envp pname')


init_s :: State
init_s a = 0

blocktest :: Stm
blocktest = Block [("x",N 1),("y",N 2)] [("p",Ass "x" (Add (V "x") (N 1)))] (Comp (Call "p") (Ass "x" (Add (V "x") (N 1))))

scopetest :: Stm
scopetest = Block [("x",N 0)] [("p",Ass "x" (Mult (V "x") (N 2))),("q",Call "p")] (Block [("x",N 5)] [("p",Ass "x" (Add (V "x") (N 1)))] (Comp (Call "q") (Ass "y" (V "x"))))

test_state :: State
test_state _ = 0

test_state_x_7 :: State
test_state_x_7 "x" = 7
test_state_x_7 _ = 0

test_state_x_14 :: State
test_state_x_14 "x" = 14
test_state_x_14 _ = 0

-- static: y=5; dynamic: y=6; mixed: y=10
scope_stm = parse " begin var x:= 0; var y:=0; proc p is x:=x*2; proc q is call p; begin var x := 5; proc p is x := x+1; call q; y := x end end"

-- x=11
recursive_stm = parse " begin var x:=1;    proc fac1 is begin     if x<=10 then       x:=x+1;       call fac1     else       skip   end;  call fac1 end"


-- Even varible should be 1 if x is even else 0
-- Note load x via a state
mutal_recursion_stm = parse "\
\begin var even:=0; \
  \proc even is begin \
    \if x=0 then \
        \even:=1 \
    \else \
        \x:=x-1; \
        \call odd \
  \end; \
  \proc odd is begin \
    \if x=0 then \
        \even:=0 \
    \else \
        \x:=x-1; \
        \call even \
    \end; \
    \call even \
\end"

test_dynamic_scope = (s_dynamic scope_stm test_state) "y" == 6
test_dynamic_recusion = (s_dynamic recursive_stm test_state) "x" == 11
test_dynamic_mutal_recusion = ((s_dynamic mutal_recursion_stm test_state_x_7) "even" == 0) && ((s_dynamic mutal_recursion_stm test_state_x_14) "even" == 1)
