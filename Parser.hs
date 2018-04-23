module Parser where

import Control.Monad 
import Text.Parsec hiding (crlf)
import Text.Parsec.String
import Control.Applicative ((<*))
import VarDef
import FuncDef
import Debug.Trace



--Calls parse on src expecting at least one univ term
parseExp :: String -> Either ParseError Multi
parseExp src = fmap multiChange (parse (many1 uParse <* eof) "" src)


-- maps changing association over the multiverse.
multiChange :: Multi -> Multi
multiChange [] = []
multiChange ((str,progs):ms) =
                 ((str,fmap stmtAsso progs):multiChange ms)

-- maps changing association over statements
stmtAsso :: Stmt -> Stmt
stmtAsso (SDecl str e) =  (SDecl str (assocSwitch e))
stmtAsso (SWhile e stmlist) =
                (SWhile (assocSwitch e)
                (map stmtAsso stmlist))
stmtAsso (SIf e stmlist1 stmlist2) =
                (SIf (assocSwitch e)
                (map stmtAsso stmlist1)
                (map stmtAsso stmlist2))
stmtAsso (SPrint str)  = (SPrint str)
stmtAsso (SPortal str) = (SPortal str)

-- Changes an Expresion that is right associative
-- into one which is left associative
assocSwitch :: Exp -> Exp
assocSwitch (EIntLit n) = (EIntLit n)
assocSwitch (EBoolLit b) = (EBoolLit b)
assocSwitch (EUOp Neg e) = (EUOp Neg (assocSwitch e))
assocSwitch (EUOp Not e) = (EUOp Not (assocSwitch e))
assocSwitch (EParens  e) = (EParens (assocSwitch e))
assocSwitch (EBin Add e1 (EBin Add e2 e3)) =
            assocSwitch (EBin Add (EBin Add e1 e2) e3)
assocSwitch (EBin Add e1 (EBin Sub e2 e3)) =
            assocSwitch (EBin Sub (EBin Add e1 e2) e3)
assocSwitch (EBin Sub e1 (EBin Add e2 e3)) =
            assocSwitch (EBin Add (EBin Sub e1 e2) e3)
assocSwitch (EBin Sub e1 (EBin Sub e2 e3)) =
            assocSwitch (EBin Sub (EBin Sub e1 e2) e3)
assocSwitch (EBin Mul e1 (EBin Mul e2 e3)) =
            assocSwitch (EBin Mul (EBin Mul e1 e2) e3)
assocSwitch (EBin Mul e1 (EBin Div e2 e3)) =
            assocSwitch (EBin Div (EBin Mul e1 e2) e3)
assocSwitch (EBin Mul e1 (EBin Mod e2 e3)) =
            assocSwitch (EBin Mod (EBin Mul e1 e2) e3)
assocSwitch (EBin Div e1 (EBin Mul e2 e3)) =
            assocSwitch (EBin Mul (EBin Div e1 e2) e3)
assocSwitch (EBin Div e1 (EBin Div e2 e3)) =
            assocSwitch (EBin Div (EBin Div e1 e2) e3)
assocSwitch (EBin Div e1 (EBin Mod e2 e3)) =
            assocSwitch (EBin Mod (EBin Div e1 e2) e3)
assocSwitch (EBin Mod e1 (EBin Mul e2 e3)) =
            assocSwitch (EBin Mul (EBin Mod e1 e2) e3)
assocSwitch (EBin Mod e1 (EBin Div e2 e3)) =
            assocSwitch (EBin Div (EBin Mod e1 e2) e3)
assocSwitch (EBin Mod e1 (EBin Mod e2 e3)) =
            assocSwitch (EBin Mod (EBin Mod e1 e2) e3)
assocSwitch (EIf cond e1 e2)= (EIf (assocSwitch cond)
                                  (assocSwitch e1)
                                  (assocSwitch e2))
assocSwitch (EVar str)     =  (EVar str)                               
assocSwitch (EBin op e1 e2) = (EBin op (assocSwitch e1) (assocSwitch e2))


main :: IO ()
main = do
  file <- getContents
  print $ parseExp file