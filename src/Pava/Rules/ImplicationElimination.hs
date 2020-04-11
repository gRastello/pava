module Pava.Rules.ImplicationElimination (checkImplicationElimination) where

import           Pava.Types
import           Pava.Util
import           Pava.Rules.Util

import qualified Control.Monad.State as S
import qualified Data.Map.Strict     as M
import           Lens.Micro
import           Prelude             hiding (id)

-- Check that an implication elimination (modus ponens) is correct.
-- The checks to perform are:
-- + we have exactly two arguments,
-- + the arguments refer to existing steps,
-- + no self reference
-- + the step depends on the same assumptions that the arguments depend on,
-- + the first argument is a ⇒-formula,
-- + the second argument is the formula that is the antecedent of the first
--   argument,
-- + the step's formula is the consequent of the first argument implication.
checkImplicationElimination :: Step -> S.State PavaState (Maybe PavaError)
checkImplicationElimination s = do
  stepMap <- S.get
  let c = [ checkArgumentNumber 2 s
          , checkArgumentsExist s stepMap
          , checkSelfReference s
          , checkDependenciesUnion s stepMap
          , (not $ checkImplication s stepMap, return . Just $ implicationError s)
          , (not $ checkAntecedend s stepMap,  return . Just $ antecedentError s)
          , (not $ checkConsequent s stepMap,  return . Just $ consequentError s)
          ]
  select (return Nothing) c

checkImplication :: Step -> PavaState -> Bool
checkImplication s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just step -> isImplication $ step^.formula

checkAntecedend :: Step -> PavaState -> Bool
checkAntecedend s stepMap =
  let args = s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just [a1, a2] -> a2^.formula == f (a1^.formula)
      where
        f :: Formula -> Formula
        f (Implication f1 f2) = f1
        f _ = error "Something terrible has happened!" -- This should never happen as for the tests we do before.

checkConsequent :: Step -> PavaState -> Bool
checkConsequent s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just step -> s^.formula == f (step^.formula)
      where
        f :: Formula -> Formula
        f (Implication f1 f2) = f2
        f _ = error "Something terrible has happened!" -- This should never happen as for the tests we do before.

implicationError :: Step -> PavaError
implicationError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe first argument is not an implication!"

antecedentError :: Step -> PavaError
antecedentError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe second argument is not the antecedent of the first!"

consequentError :: Step -> PavaError
consequentError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe formula is not the consequent of the first argument!"
