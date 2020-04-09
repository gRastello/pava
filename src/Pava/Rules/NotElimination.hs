module Pava.Rules.NotElimination (checkNotElimination) where

import           Pava.Types
import           Pava.Util
import           Pava.Rules.Util

import qualified Control.Monad.State as S
import qualified Data.Map.Strict     as M
import           Lens.Micro

-- Check that a not elimination step is correct.
-- The checks we perform are:
-- + we have exactly one argument,
-- + that argument refers to an existing step,
-- + the argument is not a self-reference,
-- + the step depends on the same assumptions that the argument depens on,
-- + the argument is a double negation,
-- + the step's formula is the double negate formula of the step.
checkNotElimination :: Step -> S.State PavaState (Maybe PavaError)
checkNotElimination s = do
  stepMap <- S.get
  let c = [ checkArgumentNumber 1 s
          , checkArgumentsExist s stepMap
          , checkSelfReference s
          , checkDependenciesUnion s stepMap
          , (not $ argumentIsDoubleNegation s stepMap, return . Just $ argumentNotDoubleNegationError s)
          , (not $ formulaMatch s stepMap,             return . Just $ formulaDoNotMatchError s)
          ]
  select (return Nothing) c

argumentIsDoubleNegation :: Step -> PavaState -> Bool
argumentIsDoubleNegation s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing   -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just step -> f $ step^.formula
      where
        f :: Formula -> Bool
        f (Not (Not _)) = True
        f _ = False

formulaMatch :: Step -> PavaState -> Bool
formulaMatch s stepMap =
  let args = s^.rule.arguments in
  case M.lookup (head args) stepMap of
    Nothing   -> error "Something terrible has happened!" -- This should never happen as for the tests we do before.
    Just step -> f $ step^.formula
      where
        f :: Formula -> Bool
        f (Not (Not x)) = x == s^.formula
        f _ = error "Something terrible has happened!" -- This should never happen as for the tests we do before.
          
argumentNotDoubleNegationError :: Step -> PavaError
argumentNotDoubleNegationError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe arugument's step is not a double negation!"

formulaDoNotMatchError :: Step -> PavaError
formulaDoNotMatchError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nFormulæ do not match!"
