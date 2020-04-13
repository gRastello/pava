module Pava.Rules.OrElimination (checkOrElimination) where

import           Pava.Types
import           Pava.Util
import           Pava.Rules.Util

import qualified Control.Monad.State as S
import qualified Data.Map.Strict     as M
import           Lens.Micro
import           Prelude             hiding (id)

-- Check that an or elimination is correct.
-- The checks to perform are:
-- + there are exactly two arguments,
-- + the arguments refer to existing steps,
-- + there is no self-reference,
-- + the step depends on the same assumptions as the argument steps,
-- + the first argument is a ∨-formula
-- + the second argument is the negation of one of the two disjuncts of the first
--   argument,
-- + the step's formula is the other conjunct of the first argument.
checkOrElimination :: Step -> S.State PavaState (Maybe PavaError)
checkOrElimination s = do
  stepMap <- S.get
  let c = [ checkArgumentNumber 2 s
          , checkArgumentsExist s stepMap
          , checkSelfReference s
          , checkDependenciesUnion s stepMap
          , (not $ firstIsOr s stepMap,   return . Just $ firstIsOrError s)
          , (not $ negation s stepMap,    return . Just $ negationError s)
          , (not $ stepFormula s stepMap, return . Just $ stepFormulaError s)
          ]
  select (return Nothing) c

firstIsOr :: Step -> PavaState -> Bool
firstIsOr s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just step -> isOr (step^.formula)

negation :: Step -> PavaState -> Bool
negation s stepMap =
  let args = s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just [s1, s2] -> f (s1^.formula) (s2^.formula) || g (s1^.formula) (s2^.formula)
      where
        f :: Formula -> Formula -> Bool
        f (Or c1 c2) (Not x) = c1 == x
        f _ _ = error "Something terrible has happened!"
        
        g :: Formula -> Formula -> Bool
        g (Or c1 c2) (Not x) = c2 == x
        g _ _ = error "Something terrible has happened!"
  
stepFormula :: Step -> PavaState -> Bool
stepFormula s stepMap =
  let args = s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just [s1, s2] -> f (s1^.formula) (s2^.formula) || g (s1^.formula) (s2^.formula)
      where
        f :: Formula -> Formula -> Bool
        f (Or c1 c2) (Not x) = c1 == x && c2 == s^.formula
        f _ _ = error "Something terrible has happened!"
        
        g :: Formula -> Formula -> Bool
        g (Or c1 c2) (Not x) = c2 == x && c1 == s^.formula
        g _ _ = error "Something terrible has happened!"

negationError :: Step -> PavaError
negationError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe second argument must be the negation of one of the two conjuncts!"

firstIsOrError :: Step -> PavaError
firstIsOrError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe first argument is not a disjunction!"

stepFormulaError :: Step -> PavaError
stepFormulaError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe formula is not the correct conjunct of the first argument (nor a conjunct of the first argument at all)!"
