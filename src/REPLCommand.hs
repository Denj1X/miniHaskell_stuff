module REPLCommand where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec.Token
import Text.Parsec (anyChar)
import Control.Applicative (some, many, (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String
  deriving (Show)

replDef :: LanguageDef st
replDef = emptyDef {
    reservedNames = [":load", ":quit"],
    reservedOpNames = [":l", ":q"]
}

repl :: TokenParser st
repl = makeTokenParser replDef

replQuit :: Parser REPLCommand
replQuit = pure Quit <$> (((reservedOp repl) ":q") <|> ((reserved repl) ":quit"))

replLoad :: Parser REPLCommand
replLoad = do
  (((reserved repl) ":load") <|> ((reservedOp repl) ":l") )
  s <- some anyChar
  return $ Load s

replEval :: Parser REPLCommand
replEval = Eval <$> many anyChar

replCommand :: Parser REPLCommand
replCommand = replQuit
          <|> replLoad
          <|> replEval
