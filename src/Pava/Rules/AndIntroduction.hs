module Pava.Rules.AndIntroduction (checkAndIntroduction) where

import           Pava.Types
import           Pava.Util
import           Pava.Rules.Util

import qualified Data.Map.Strict     as M
import qualified Control.Monad.State as S
import           Lens.Micro
import           Prelude             hiding (id)

-- Check that an and intruction is correct.
-- The checks to perform are:
-- + the formula is really a ∧-formula,
-- + we have exactly two arguments,
-- + the argumepnts refer to existing steps,
-- + the arguments should not contain the step's own id,
-- + the step depends on the union of the assumptions of the two agument steps
--   (or on their id in case they are assumptions),
-- + the two subformulæ should be the ones in the arguments steps.
checkAndIntroduction :: Step -> S.State PavaState (Maybe PavaError)
checkAndIntroduction s = do
  stepMap <- S.get
  let c = [ checkArgumentNumber 2 s
          , checkArgumentsExist s stepMap
          , checkSelfReference s
          , checkDependenciesUnion s stepMap
          , (not . isAnd $ _formula s,         return . Just $ notAndError s)
          , (not $ checkSubformulae s stepMap, return . Just $ subformulaeError s)
          ]
  select (return Nothing) c

-- Returns True if the dependencies are correct in the given step.
checkDepends :: Step -> M.Map Integer Step -> Bool
checkDepends s stepMap =
  let args = s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing    -> error "Something terrible has happened!" -- This should never happen as for the checks we do before.
    Just steps -> concat (f <$> steps) == s^.dependsOn
      where
        f :: Step -> [Integer]
        f s = if s^.rule.name == Assumption
          then [s^.id]
          else s^.dependsOn

-- CHeck that the subformulae of the formula of the step are the same as the ones in the argument steps.
checkSubformulae :: Step -> M.Map Integer Step -> Bool
checkSubformulae s stepMap =
  let args = s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing -> error "Something terrible has happended!" -- This should never happen as for the checks we do before.
    Just ss -> f (s^.formula) ss
      where
        f :: Formula -> [Step] -> Bool
        f (And f1 f2) ss = [f1, f2] == (_formula <$> ss)
        f _ _ = error "Something terrible has happended!" -- This should never happen as for the checks we do before.

notAndError :: Step -> PavaError
notAndError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe formula is not a conjuction!"

subformulaeError :: Step -> PavaError
subformulaeError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe formulæ do not match!"
