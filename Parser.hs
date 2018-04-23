module Parser where
import VarDef
import FuncDef
import Debug.Trace
import Control.Monad 
import Text.Parsec.String
import Text.Parsec hiding (crlf)
import Control.Applicative ((<*))


parseExpressions :: String -> Either ParseError Multi
parseExpressions src = fmap multiChange (parse (many1 uParse <* eof) "" src)


multiChange :: Multi -> Multi
multiChange [] = []
multiChange ((str,progs):ms) = ((str,fmap statementAssoc progs):multiChange ms)


statementAssoc :: Stmt -> Stmt
statementAssoc (SDecl str e) =  (SDecl str (switchAssociation e))
statementAssoc (SWhile e stmlist) =
                (SWhile (switchAssociation e)
                (map statementAssoc stmlist))
statementAssoc (SIf e stmlist1 stmlist2) =
                (SIf (switchAssociation e)
                (map statementAssoc stmlist1)
                (map statementAssoc stmlist2))
statementAssoc (SPrint str)  = (SPrint str)
statementAssoc (SPortal str) = (SPortal str)


switchAssociation :: Exp -> Exp
switchAssociation (EIntLit n) = (EIntLit n)
switchAssociation (EBoolLit b) = (EBoolLit b)
switchAssociation (EParens  e) = (EParens (switchAssociation e))
switchAssociation (EBin Add e1 (EBin Add e2 e3)) =
            switchAssociation (EBin Add (EBin Add e1 e2) e3)
switchAssociation (EBin Add e1 (EBin Sub e2 e3)) =
            switchAssociation (EBin Sub (EBin Add e1 e2) e3)
switchAssociation (EBin Sub e1 (EBin Add e2 e3)) =
            switchAssociation (EBin Add (EBin Sub e1 e2) e3)
switchAssociation (EBin Sub e1 (EBin Sub e2 e3)) =
            switchAssociation (EBin Sub (EBin Sub e1 e2) e3)
switchAssociation (EBin Mul e1 (EBin Mul e2 e3)) =
            switchAssociation (EBin Mul (EBin Mul e1 e2) e3)
switchAssociation (EBin Mul e1 (EBin Div e2 e3)) =
            switchAssociation (EBin Div (EBin Mul e1 e2) e3)
switchAssociation (EBin Mul e1 (EBin Mod e2 e3)) =
            switchAssociation (EBin Mod (EBin Mul e1 e2) e3)
switchAssociation (EBin Div e1 (EBin Mul e2 e3)) =
            switchAssociation (EBin Mul (EBin Div e1 e2) e3)
switchAssociation (EBin Div e1 (EBin Div e2 e3)) =
            switchAssociation (EBin Div (EBin Div e1 e2) e3)
switchAssociation (EBin Div e1 (EBin Mod e2 e3)) =
            switchAssociation (EBin Mod (EBin Div e1 e2) e3)
switchAssociation (EBin Mod e1 (EBin Mul e2 e3)) =
            switchAssociation (EBin Mul (EBin Mod e1 e2) e3)
switchAssociation (EBin Mod e1 (EBin Div e2 e3)) =
            switchAssociation (EBin Div (EBin Mod e1 e2) e3)
switchAssociation (EBin Mod e1 (EBin Mod e2 e3)) =
            switchAssociation (EBin Mod (EBin Mod e1 e2) e3)
switchAssociation (EIf cond e1 e2)= (EIf (switchAssociation cond)
                                  (switchAssociation e1)
                                  (switchAssociation e2))
switchAssociation (EVar str)     =  (EVar str)                               
switchAssociation (EBin op e1 e2) = (EBin op (switchAssociation e1) (switchAssociation e2))


main :: IO ()
main = do
  file <- getContents
  print $ parseExpressions file