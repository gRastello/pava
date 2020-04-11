module Pava.Lib (pava, check) where

import           Pava.Util
import           Pava.Rules.Assumption
import           Pava.Rules.AndIntroduction
import           Pava.Rules.AndElimination
import           Pava.Rules.NotIntroduction
import           Pava.Rules.NotElimination
import           Pava.Rules.ImplicationElimination
import           Pava.Types

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as To

import qualified Control.Monad.State                    as S
import qualified Data.Map.Strict                        as M
import           Lens.Micro
import           Prelude                                hiding (id)

--------------
-- Parsers. --
--------------
pavaDef = emptyDef { To.commentLine     = "#"
                   , To.reservedOpNames = ["∧", "¬", "⇒"]
                   , To.reservedNames   = ["a", "I∧", "E∧", "I¬", "E¬", "I⇒", "E⇒"]
                   }

lexer = To.makeTokenParser pavaDef

operator   = To.reservedOp lexer
parens     = To.parens     lexer
reserved   = To.reserved   lexer
natural    = To.natural    lexer
comma      = To.comma      lexer
semi       = To.semi       lexer
whiteSpace = To.whiteSpace lexer

variableP :: Parser Formula
variableP = do
  var <- many1 letter
  whiteSpace
  return $ Variable var

formulaP :: Parser Formula
formulaP = buildExpressionParser operators termP

termP :: Parser Formula
termP = parens formulaP <|> variableP

operators = [ [Prefix (operator "¬" >> return Not)]
            , [Infix  (operator "∧" >> return And) AssocLeft]
            , [Infix  (operator "⇒" >> return Implication) AssocLeft]
            ]

ruleName :: Parser RuleName
ruleName = choice $ fmap f ["a", "I∧", "E∧", "I¬", "E¬", "I⇒", "E⇒"]
  where f "a"  = reserved "a"  >> return Assumption
        f "I∧" = reserved "I∧" >> return AndIntroduction
        f "E∧" = reserved "E∧" >> return AndElimination
        f "I¬" = reserved "I¬" >> return NotIntroduction
        f "E¬" = reserved "E¬" >> return NotElimination
        f "I⇒" = reserved "I⇒" >> return ImplicationIntroduction
        f "E⇒" = reserved "E⇒" >> return ImplicationElimination

ruleArguments :: Parser [Integer]
ruleArguments = parens $ natural `sepBy1` comma

ruleP :: Parser Rule
ruleP = do
  name <- ruleName
  whiteSpace
  arguments <- ruleArguments <|> return []
  whiteSpace
  return $ Rule name arguments

stepP :: Parser Step
stepP = do
  i <- natural
  f <- formulaP
  r <- ruleP
  d <- natural `sepBy` comma
  semi
  return $ Step i f r d

pava :: Parser [Step]
pava = whiteSpace >> many1 stepP

----------------------
-- The actual Pava. --
----------------------
check :: [Step] -> String
check ss = case fst $ S.runState (checkProof ss) M.empty of
  Nothing  -> "The proof is correct!"
  Just err -> err

checkProof :: [Step] -> S.State PavaState (Maybe PavaError)
checkProof [] = return Nothing
checkProof (x:xs) = do
  y <- checkStep x
  case y of
    Nothing  -> checkProof xs
    Just err -> return . Just $ err

-- Checks if a new step is fine. Return a Nothing in the state if everything
-- went fine and an error if not (we'll handle the error somewhere else).
checkStep :: Step -> S.State PavaState (Maybe PavaError)
checkStep s = do
  stepMap <- S.get
  if M.member (s^.id) stepMap
    then return . Just $ sameIdError s
    else do
      S.put $ M.insert (s^.id) s stepMap
      checkStep' s

-- Branches into a different checking function for each rule.
checkStep' :: Step -> S.State PavaState (Maybe PavaError)
checkStep' s = select (error "something dreadful has happened.") c
  where c = [ ((s^.rule.name) == Assumption,      checkAssumption s)
            , ((s^.rule.name) == AndIntroduction, checkAndIntroduction s)
            , ((s^.rule.name) == AndElimination,  checkAndElimination s)
            , ((s^.rule.name) == NotIntroduction, checkNotIntroduction s)
            , ((s^.rule.name) == NotElimination,  checkNotElimination s)
            , ((s^.rule.name) == ImplicationElimination,  checkImplicationElimination s)
            ]

sameIdError :: Step -> PavaError
sameIdError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nAnother step with the same id already exists!"

---------------------------------
-- Examples and testing stuff. --
---------------------------------
testDerivation :: String
testDerivation =
     "1 A⇒B a;"
  ++ "2 A a;"
  ++ "3 B E⇒(1, 2) 1, 2;"
  --    "1 A∧¬A a;\n"
  -- ++ "2 ¬A  E∧(1) 1;\n"
  -- ++ "3 A E∧(1) 1;\n"
  -- ++ "4 ¬B a;\n"
  -- ++ "5 ¬(¬B) I¬(4, 2, 3) 1;\n"
  -- ++ "6 B E¬(5) 1;"
-- testDerivation =
  --    "1 A a;\n"
  -- ++ "2 ¬B a;\n"
  -- ++ "#This is a line comment.\n"
  -- ++ "3 A∧¬B I∧(1, 2) 1, 2;\n"
  -- ++ "4 A∧A  I∧(1, 1) 1;\n"
  -- ++ "5 ¬B E∧(3) 1, 2;\n"
  -- ++ "6 A  E∧(3) 1, 2;\n"
  -- ++ "7 ¬(¬(C ∧ D)) a;\n"
  -- ++ "8 C ∧ D E¬(7) 7;\n"
  -- ++ "9 C E∧(8) 7;\n"

ultimateTesting :: IO ()
ultimateTesting = case parse pava "test derivation" testDerivation of
    Left  err -> print err
    Right ss  -> putStrLn $ check ss
