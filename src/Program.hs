module Program where

import Control.Applicative (Alternative (..))
import Data.Map.Strict.Internal qualified as Map
import Eval (substitute)
import Exp
import Lab2 (Parser, endOfInput, reserved, semi, semiSep1, whiteSpace)
import Parsing (expr, parseFirst, var)
import Sugar (desugarExp, desugarVar)
import System.IO (hPutStrLn, stderr)

data Definition = Definition
  { defHead :: Var,
    defArgs :: [Var],
    defBody :: ComplexExp
  }
  deriving (Show)

definition :: Parser Definition
definition = do
  head <- var
  args <- many var
  reserved ":="
  Definition head args <$> expr

-- >>> parseFirst definition "id := \\x -> x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [], defBody = CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))})

-- >>> parseFirst definition "id x := x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})})

-- >>> parseFirst definition "const x y := x"
-- Just (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})

program :: Parser [Definition]
program = whiteSpace *> semiSep1 definition <* semi <* endOfInput

-- >>> parseFirst program "    id x := x ; const x y := x"
-- Nothing

-- >>> parseFirst program "    id x := x ; const x y := x ;"
-- Just [Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})},Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})}]

definitionExp :: Definition -> ComplexExp
definitionExp def = foldr CLam (defBody def) (defArgs def)

-- >>> definitionExp (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})
-- CLam (Var {getVar = "x"}) (CLam (Var {getVar = "y"}) (CX (Var {getVar = "x"})))

type Environment = Map.Map IndexedVar Exp

programEnv :: [Definition] -> Environment
programEnv pgm = Map.fromList [(desugarVar (defHead def), desugarExp (definitionExp def)) | def <- pgm]

normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv env v = maybe v (normalizeEnv env) (step v)
  where
    step (X x) = Map.lookup x env
    step (Lam x v) = Lam x <$> step v
    step (App (Lam x ex) v) = Just (substitute x v ex)
    step (App e1 e2) =
      case step e1 of
        Nothing -> App e1 <$> step e2
        Just e1' -> Just (App e1' e2)