module Pava.Rules.Util ( checkArgumentNumber
                       , checkArgumentsExist
                       , checkSelfReference
                       , checkDependenciesUnion
                       , setEqual
                       , setMinus
                       ) where

import           Pava.Types
import           Pava.Util

import           Prelude             hiding (id)
import           Lens.Micro
import qualified Control.Monad.State as S
import qualified Data.Map.Strict     as M

checkArgumentNumber :: Int -> Step -> (Bool, S.State PavaState (Maybe PavaError))
checkArgumentNumber n s = (length (s^.rule.arguments) /= n, return . Just $ argumentNumberError s n)

argumentNumberError :: Step -> Int -> PavaError
argumentNumberError s n = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nExpected " ++ show n ++ " arguments (no more, no less)!"

checkArgumentsExist :: Step -> PavaState -> (Bool, S.State PavaState (Maybe PavaError))
checkArgumentsExist s stepMap = (not $ (s^.rule.arguments) `members` stepMap, return . Just $ argumentsExistsError s)

argumentsExistsError :: Step -> PavaError
argumentsExistsError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nSome of the required steps could not be found at this point in the proof!"

checkSelfReference :: Step -> (Bool, S.State PavaState (Maybe PavaError))
checkSelfReference s = ((s^.id) `elem` s^.rule.arguments, return . Just $ selfReferenceError s)

selfReferenceError :: Step -> PavaError
selfReferenceError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nCannot use the step itself as an argument!"

-- Check that the dependencies of the step are exactly the union of the dependcies
-- of the argument steps plus eventual assumptions.
-- NOTE: this functions assumes that existence of the arguments in the environment
-- has already been verified by some other mean (suck as the functions above).
-- Otherwise the (*) error below will be executed terminating the whole thing.
checkDependenciesUnion :: Step -> PavaState -> (Bool, S.State PavaState (Maybe PavaError))
checkDependenciesUnion s stepMap = (not $ dependOnUnion s stepMap, return . Just $ dependOnUnionError s)

dependOnUnion :: Step -> PavaState -> Bool
dependOnUnion s stepMap =
  let args = s^.rule.arguments in
  case mapM (`M.lookup` stepMap) args of
    Nothing    -> error "Something terrible has happened!" -- (*)
    Just steps -> concat (f <$> steps) `setEqual` (s^.dependsOn)
      where
        f :: Step -> [Integer]
        f s = if s^.rule.name == Assumption
          then [s^.id]
          else s^.dependsOn

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset (x:xs) ys = (x `elem` ys) && (xs `isSubset` ys)
isSubset []     ys = True

setEqual :: Eq a => [a] -> [a] -> Bool
setEqual x y = x `isSubset` y && y `isSubset` x

setMinus :: Eq a => [a] -> a -> [a]
setMinus [] _ = []
setMinus (x:xs) y = if x == y then setMinus xs y else x : setMinus xs y

dependOnUnionError :: Step -> PavaError
dependOnUnionError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nThe step must depend on all the assumptions on which the argument steps"
  ++ " depend on, plus eventual assumptions. But on nothing more than that!"
