module Pava.Rules.AndElimination (checkAndElimination) where

import           Pava.Types
import           Pava.Util
import           Pava.Rules.Util

import qualified Control.Monad.State as S
import qualified Data.Map.Strict     as M
import           Lens.Micro
import           Prelude             hiding (id)

-- Check that an and elimination step is correct.
-- The checks to perform are:
-- + we have exactly one argument,
-- + that argument refers to an existing step,
-- + the argument is not the step's own id,
-- + that argument is a ∧-formula,
-- + the step depends on the same assumptions that the argument depens on,
-- + the step's formula is one of the two subformulae of the argument step.
checkAndElimination :: Step -> S.State PavaState (Maybe PavaError)
checkAndElimination s = do
  stepMap <- S.get
  let c = [ checkArgumentNumber 1 s
          , checkArgumentsExist s stepMap
          , checkSelfReference s
          , checkDependenciesUnion s stepMap 
          , (not $ argumentIsAnd s stepMap,    return . Just $ argumentNotAndError s)
          , (not $ checkSubformulae s stepMap, return . Just $ subformulaeError s)
          ]
  select (return Nothing) c

argumentIsAnd :: Step -> PavaState -> Bool
argumentIsAnd s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just s  -> isAnd $ s^.formula

-- Return true if the dependencies of the step are exactly the union (note ordering problems)
-- of those of the argument steps plus eventual assumptions.
--
-- TODO: move this in a rule-checking-util file and make it so that the order does not matter and duplicates are removed. 
dependOnUnion :: Step -> M.Map Integer Step -> Bool
dependOnUnion s stepMap =
  let args = s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing    -> error "Something terrible has happened!" -- This should never happen as for the checks we do before.
    Just steps -> concat (f <$> steps) == s^.dependsOn
      where
        f :: Step -> [Integer]
        f s = if (s^.rule.name) == Assumption
          then [s^.id]
          else s^.dependsOn

checkSubformulae :: Step -> M.Map Integer Step -> Bool
checkSubformulae s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just s' -> f $ s'^.formula
      where
        f :: Formula -> Bool
        f (And f1 f2) = (f1 == s^.formula) || (f2 == s^.formula)
        f _ = error "Something terrible has happened!" -- This should never happen as for the tests we do before.

argumentNotAndError :: Step -> PavaError
argumentNotAndError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe argument's formula is not a conjunction!"

subformulaeError :: Step -> PavaError
subformulaeError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe formula is not a conjunct of the step's one!"
