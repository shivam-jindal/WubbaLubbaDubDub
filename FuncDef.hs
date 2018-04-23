module FuncDef where
import VarDef
import Debug.Trace
import Control.Monad 
import Text.Parsec.String
import Control.Applicative ((<*))
import Text.Parsec hiding (crlf)


--GRAMMAR--

--multi -> [univ]
--univ -> [stmt]
--stmt ->  decl  | print | if | while
--if -> 'if' opb 'then' [stmt] 'otherwise' [stmt]

--expr -> op1

--op1  -> op2 and op1 | op2 or op1  | op2
--op2  -> op3 == op2  | op3 < op2   | op3 > op2  | op3
--op3  -> op4 + op3   | op4 - op3   | op4
--op4  -> pterm * op4 | pterm / op4 | pterm mod op4 | pterm
--pterm -> base | parenA | parenB | pvar
--base -> pint | bRight | bWrong


whitespace = void $ many $ oneOf" \t\n"
crlf = many $ oneOf "\n"

--Parsing for arithmetic operations
pmul = do
  whitespace
  e1 <- pterm
  whitespace
  string "times"
  whitespace
  e2 <- op4
  whitespace
  return $ EBin Mul e1 e2

pdiv = do
  whitespace
  e1 <- pterm
  whitespace
  string "divided by"
  whitespace
  e2 <- op4
  whitespace
  return $ EBin Div e1 e2

pmod = do
  whitespace
  e1 <- pterm
  whitespace
  string "mod"
  whitespace
  e2 <- op4
  whitespace
  return $ EBin Mod e1 e2

padd = do
  whitespace
  e1 <- op4
  whitespace
  string "plus"
  whitespace
  e2 <- op3
  whitespace
  return $ EBin Add e1 e2

psub = do
  whitespace
  e1 <- op4
  whitespace
  string "minus"
  whitespace
  e2 <- op3
  whitespace
  return $ EBin Sub e1 e2

band = do
  whitespace
  e1 <- op2
  whitespace
  string "and"
  whitespace
  e2 <- op1
  whitespace
  return $ EBin And e1 e2

bor = do
  whitespace
  e1 <- op2
  whitespace
  string "or"
  whitespace
  e2 <- op1
  whitespace
  return $ EBin Or e1 e2

numEq = do
  whitespace
  e1 <- op3
  whitespace
  string "is the same as"
  e2 <- op3
  whitespace
  return $ EBin Equals e1 e2

numLt = do
  whitespace
  e1 <- op3
  whitespace
  string "is less than"
  e2 <- op3
  whitespace
  return $ EBin LessThan e1 e2

numGt = do
  whitespace
  e1 <- op3
  whitespace
  string "is greater than"
  e2 <- op3
  whitespace
  return $ EBin LessThan e1 e2


--Parsing parent statements
parenA = do
  whitespace
  void $ string "you gotta"
  whitespace
  e <- op3
  whitespace
  void $ string "Morty"
  whitespace
  return (EParens e)

parenB = do
  whitespace
  void $ string "you gotta"
  whitespace
  e <- op1
  whitespace
  void $ string "Morty"
  whitespace
  return (EParens e)


--Parsing statements
sIf = do
  whitespace
  string "if"
  whitespace
  e1 <- op1
  whitespace
  string "then"
  whitespace
  s1 <- stmt `endBy` crlf
  whitespace
  string "otherwise"
  whitespace
  s2 <- stmt `endBy` crlf
  whitespace
  string "wubalubadubdub"
  whitespace
  return $ SIf e1 s1 s2

sDec = do
  whitespace
  s <- many1 letter
  whitespace
  string "squanch"
  whitespace
  e <- expr
  whitespace
  return $ SDecl s e

sPrint = do
  whitespace
  string "show me what you got"
  whitespace
  s <- many1 letter
  whitespace
  return $ SPrint s

sWhile = do
  whitespace
  string "while"
  whitespace
  e <- op1
  whitespace
  string "do this for grandpa"
  whitespace
  s <- stmt `endBy` crlf
  whitespace
  string "thanks Summer"
  whitespace
  return $ SWhile e s

sPortal = do
  whitespace
  string "lets grab our"
  whitespace
  u <- many1 letter
  whitespace
  string "and portal out of here"
  whitespace
  return $ SPortal u

uParse = do
  whitespace
  string "listen"
  whitespace
  s <- many1 letter
  whitespace
  b <- stmt `endBy` crlf
  whitespace
  return $ (s, b)


stmt = try sPortal <|> try sIf <|> try sDec <|> try sPrint <|> try sWhile

expr = try op1

op1 = try band <|> try bor <|> op2
op2 = try numEq <|> try numLt <|> try numGt <|> op3
op3 = try padd <|> try psub <|> op4
op4 = try pmul <|> try pdiv <|> try pmod  <|> pterm
pterm = try base <|> try parenA <|> try parenB <|> try pvar
  where base = pint <|> try bRight <|> try bWrong

pint :: Parser Exp
pint = do
  whitespace
  n <- many1 digit
  whitespace
  return $ EIntLit (read n)

bRight :: Parser Exp
bRight = do
  whitespace
  string "right"
  whitespace
  return $ EBoolLit True

bWrong :: Parser Exp
bWrong = do
  whitespace
  string "wrong"
  whitespace
  return $ EBoolLit False

pvar :: Parser Exp
pvar = do
  whitespace
  v <- many1 letter
  whitespace
  return $ EVar v