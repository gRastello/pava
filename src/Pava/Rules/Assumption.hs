module Pava.Rules.Assumption (checkAssumption) where

import           Pava.Types
import           Pava.Util
import           Pava.Rules.Util

import qualified Data.Map.Strict     as M
import qualified Control.Monad.State as S

-- Check that an assumption is correct.
-- There is few checking to do here. It might not be necessary but we check that
-- the assumption do not depend on any other step.
-- We also check that no arguments are given to the assumption rule as that is
-- stupid too. This might become a warning.
checkAssumption :: Step -> S.State PavaState (Maybe PavaError)
checkAssumption s = select (return Nothing) c
  where c = [ (not . null $ _dependsOn s,         return . Just $ depensOnStuffError s)
            , checkArgumentNumber 0 s
            ]

depensOnStuffError :: Step -> PavaError
depensOnStuffError s = "Error while checking step\n"
  ++ "  " ++ show s
  ++ "\nAn assumption should not depend on any other step!"
