module Pava.Rules.ImplicationIntroduction (checkImplicationIntroduction) where

import           Pava.Types
import           Pava.Util
import           Pava.Rules.Util

import qualified Data.Map.Strict     as M
import qualified Control.Monad.State as S
import           Lens.Micro
import           Prelude             hiding (id)

-- Check that an implicat inintroduction is correct.
-- The checks to perform are:
-- + we have exactly two arguments,
-- + the arguments refer to existing steps,
-- + there is no self reference,
-- + the formula is a ⇒-formula,
-- + the first argument is an assumption,
-- + the formula's antecendt is the first argument,
-- + its consequent the second argument,
-- + the formula depends on what the second argument depends on minus the first
--   argument id (the assumption is discarded).
checkImplicationIntroduction :: Step -> S.State PavaState (Maybe PavaError)
checkImplicationIntroduction s = do
  stepMap <- S.get
  let c = [ checkArgumentNumber 2 s
          , checkArgumentsExist s stepMap
          , checkSelfReference s
          , (not $ isImplication (s^.formula), return . Just $ implicationError s)
          , (not $ assumption s stepMap,       return . Just $ assumptionError s)
          , (not $ antecedent s stepMap,       return . Just $ antecedentError s)
          , (not $ consequent s stepMap,       return . Just $ consequentError s)
          , (not $ dependencies s stepMap,     return . Just $ dependenciesError s)
          ]
  select (return Nothing) c

assumption :: Step -> PavaState -> Bool
assumption s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing -> error "Something terrible has happended!" -- This should never happen as for the checks we do before.
    Just step -> step^.rule.name == Assumption

antecedent :: Step -> PavaState -> Bool
antecedent s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing -> error "Something terrible has happended!" -- This should never happen as for the checks we do before.
    Just step -> step^.formula == f (s^.formula)
      where
        f :: Formula -> Formula
        f (Implication f1 f2) = f1
        f _ = error "Something terrible has happended!"

consequent :: Step -> PavaState -> Bool
consequent s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head $ tail args) stepMap of
    Nothing -> error "Something terrible has happended!" -- This should never happen as for the checks we do before.
    Just step -> step^.formula == f (s^.formula)
      where
        f :: Formula -> Formula
        f (Implication f1 f2) = f2
        f _ = error "Something terrible has happended!"

dependencies :: Step -> PavaState -> Bool
dependencies s stepMap =
  let args = s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing -> error "Something terrible has happended!" -- This should never happen as for the checks we do before.
    Just [s1, s2] -> ((s1^.dependsOn) `setMinus` (s1^.id)) `setEqual` (s^.dependsOn)

implicationError :: Step -> PavaError
implicationError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe formula is not an implication!"

assumptionError :: Step -> PavaError
assumptionError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe first argument is not an assumption!"

antecedentError :: Step -> PavaError
antecedentError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe antecendt does not correspond with the first argument!"

consequentError :: Step -> PavaError
consequentError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe consequent does not correspond with the second argument!"

dependenciesError :: Step -> PavaError
dependenciesError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe dependencies are not correct!"
