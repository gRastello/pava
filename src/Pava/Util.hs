module Pava.Util where

import qualified Data.Map.Strict as M

-- Basically a case switch.
select :: a -> [(Bool, a)] -> a
select def []   = def
select def (x:xs) = if fst x then snd x else select def xs

-- Check if a bunch of keys all exist in a given Map.
members :: Ord k => [k] -> M.Map k a -> Bool
members keys map = and $ (`M.member` map) <$> keys
