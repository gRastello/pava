module Pava.Types where

import qualified Data.Map.Strict as M
import           Lens.Micro

data Formula = Variable String
             | And Formula Formula
             | Implication Formula Formula
             | Not Formula
             deriving Eq

instance Show Formula where
  show (Variable s) = s
  show (Not (Variable s)) = "¬" ++ s
  show (Not f) = "¬(" ++ show f ++ ")"
  show (And f1 f2) = show f1 ++ " ∧ " ++ show f2
  show (Implication f1 f2) = show f1 ++ " ⇒ " ++ show f2

-- Functions to check the kind of formula.
isVariable :: Formula -> Bool
isVariable (Variable _) = True
isVariable _ = False

isAnd :: Formula -> Bool
isAnd (And _ _) = True
isAnd _ = False

isImplication :: Formula -> Bool
isImplication (Implication _ _) = True
isImplication _ = False

isNot :: Formula -> Bool
isNot (Not _) = True
isNot _ = False

data RuleName = Assumption
              | AndIntroduction | AndElimination
              | NotIntroduction | NotElimination
              | ImplicationIntroduction | ImplicationElimination
              deriving Eq

instance Show RuleName where
  show Assumption      = "a"
  show AndIntroduction = "I∧"
  show AndElimination  = "E∧"
  show NotIntroduction = "I¬"
  show NotElimination  = "E¬"
  show ImplicationIntroduction = "I⇒"
  show ImplicationElimination = "E⇒"

data Rule = Rule { _name      :: RuleName
                 , _arguments :: [Integer]
                 }

name :: Lens Rule Rule RuleName RuleName
name = lens _name (\rule x -> rule { _name = x })

arguments :: Lens Rule Rule [Integer] [Integer]
arguments = lens _arguments (\rule x -> rule { _arguments = x })

instance Show Rule where
  show r = show (_name r) ++ " " ++ show (_arguments r)

data Step = Step { _id        :: Integer
                 , _formula   :: Formula
                 , _rule      :: Rule
                 , _dependsOn :: [Integer] -- List of assumptions the step
                                           -- depends on.
                 }

id :: Lens Step Step Integer Integer
id = lens _id (\step x -> step { _id = x })

formula :: Lens Step Step Formula Formula
formula = lens _formula (\step x -> step { _formula = x })

rule :: Lens Step Step Rule Rule
rule = lens _rule (\step x -> step { _rule = x })

dependsOn :: Lens Step Step [Integer] [Integer]
dependsOn = lens _dependsOn (\step x -> step { _dependsOn = x })

instance Show Step where
  show l = show (_id l) ++ " "
    ++ show (_formula l) ++ " "
    ++ show (_rule l) ++ " "
    ++ show (_dependsOn l)

type PavaState = M.Map Integer Step

type PavaError = String
