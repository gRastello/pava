module Pava.Rules.NotIntroduction (checkNotIntroduction) where

import           Pava.Types
import           Pava.Util
import           Pava.Rules.Util

import qualified Data.Map.Strict     as M
import           Data.List
import qualified Control.Monad.State as S
import           Lens.Micro
import           Prelude             hiding (id)

-- Check that a not introduction is correct.
-- The checks to perform are:
-- + we have exactly three arguments,
-- + the arguments refer to existing steps,
-- + the arguments do not contain the step's own id,
-- + the first argument is an assumption
-- + the step depends on the union of the assumptions of the second two
--   arguments steps but not on the first step,
-- + the second two arguments steps are one the formal negation of the other,
-- + the step's formula is the negation of the first argument formula.
checkNotIntroduction :: Step -> S.State PavaState (Maybe PavaError)
checkNotIntroduction s = do
  stepMap <- S.get
  let c = [ checkArgumentNumber 3 s
          , checkArgumentsExist s stepMap
          , checkSelfReference s
          , (not $ checkFirstAssumption s stepMap, return . Just $ firstAssumptionError s)
          , (not $ checkDependencies s stepMap,    return . Just $ dependenciesError s)
          , (not $ checkFormalNegation s stepMap,  return . Just $ formalNegationerror s)
          , (not $ checkFormula s stepMap,         return . Just $ formulaError s)
          ]
  select (return Nothing) c

-- Checks that the first argument is an assumption.
checkFirstAssumption :: Step -> M.Map Integer Step -> Bool
checkFirstAssumption s stepMap =
  let arg = head $ s^.rule.arguments in
  case M.lookup arg stepMap of
    Nothing -> error "Something terrible has happened!" -- This should never happen.
    Just step -> (step^.rule.name) == Assumption

-- Check that the dependencies are correct.
checkDependencies :: Step -> M.Map Integer Step -> Bool
checkDependencies s stepMap =
  let args = s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing -> error "Something terrible has happened!" -- Should never happen.
    Just steps -> (concat (f <$> tail steps) `setMinus` (head steps ^. id)) `setEqual` (s^.dependsOn)
      where
        f :: Step -> [Integer]
        f s | s^.rule.name == Assumption = [s^.id]
            | otherwise = s^.dependsOn

-- Check that the second and third arguments' formulae are one the formal
-- negation of the other.
checkFormalNegation :: Step -> M.Map Integer Step -> Bool
checkFormalNegation s stepMap =
  let args = tail $ s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing -> error "Something terrible has happened!" -- Should never happen.
    Just steps -> let formulae = _formula <$> steps
                      -- The following should not crash due to the previous checks.
                      f1 = head formulae
                      f2 = formulae !! 1 in
      f f1 f2 || f f2 f1
      where
        f :: Formula -> Formula -> Bool
        f (Not x) x' = x == x'
        f _ _ = False

checkFormula :: Step -> M.Map Integer Step -> Bool
checkFormula s stepMap =
  let arg = head $ s^.rule.arguments in
  case M.lookup arg stepMap of
    Nothing -> error "Something terrible has happened!" -- Should never happen.
    Just step -> Not (step^.formula) == s^.formula

firstAssumptionError :: Step -> PavaError
firstAssumptionError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe first argument is supposed to be an assumption!"


dependenciesError :: Step -> PavaError
dependenciesError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe step dependencies are wrong!"

formalNegationerror :: Step -> PavaError
formalNegationerror s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe provided formulae are not a formal contradiction!"

formulaError :: Step -> PavaError
formulaError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe formula is not the negation of the first argument's formula!"
