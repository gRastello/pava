module Pava.Rules.OrIntroduction (checkOrIntroduction) where

import           Pava.Types
import           Pava.Util
import           Pava.Rules.Util

import qualified Data.Map.Strict     as M
import qualified Control.Monad.State as S
import           Lens.Micro
import           Prelude             hiding (id)

-- Check that an or introduction is correct.
-- The checks to perform are:
-- + exactly one argument,
-- + that argument refers to an existing setep,
-- + there is no self reference,
-- + the step depends on the same assumptions as the argument step (or on the
--   argument itself if it is an assumption)
-- + the step is a ∨-formula,
-- + one of the disjuct of the formula is the argument formula.
checkOrIntroduction :: Step -> S.State PavaState (Maybe PavaError)
checkOrIntroduction s = do
  stepMap <- S.get
  let c = [ checkArgumentNumber 1 s
          , checkArgumentsExist s stepMap
          , checkSelfReference s
          , checkDependenciesUnion s stepMap
          , (not $ isOr (s^.formula),    return . Just $ orError s)
          , (not $ disjunct s stepMap, return . Just $ disjunctError s)
          ]
  select (return Nothing) c

disjunct :: Step -> PavaState -> Bool
disjunct s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing -> error "Something terrible has happended!" -- This should never happen as for the checks we do before.
    Just step -> (step^.formula == f (s^.formula)) || (step^.formula == g (s^.formula))
      where
        f :: Formula -> Formula
        f (Or x1 x2) = x1
        f _ = error "Something terrible has happended!"

        g :: Formula -> Formula
        g (Or x1 x2) = x2
        g _ = error "Something terrible has happended!"

orError :: Step -> PavaError
orError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe formula is not a disjunction!"

disjunctError :: Step -> PavaError
disjunctError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nNone of the disjuncts is the argument step's formula!"
