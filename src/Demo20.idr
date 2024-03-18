module Demo20

import Data.List.Quantifiers

data StateBuilder : List Type -> List Type -> return -> Type where
  Return : value -> StateBuilder left [] value
  (>>=) : value -> ((value, value -> HList (left ++ [value] ++ right)) -> StateBuilder (left ++ [value]) right return) -> StateBuilder left (value :: right) return

runInit : StateBuilder left right return -> HList right
runInit (Return value) = Nil
runInit ((>>=) (initial) next) = initial :: (runInit (next (initial, \newValue => ?_)))

runNext : StateBuilder left right return -> HList left -> HList right -> return
runNext (Return value) left Nil = value
runNext ((>>=) (initial) next) left (value :: rest) = runNext (next (value, \newValue => left ++ [newValue] ++ rest)) (left ++ [value]) rest where

e1 : StateBuilder [] [String, Int] String
e1 = do
  (x, setX) <- "A"
  (y, setY) <- the Int 1
  Return $ (show x) ++ (show y)

e1e = runNext e1 [] ["AA", 10]

-- e2 : StateBuilder ?e2h4 String
-- e2 = do
--   (y, setY) <- True
--   (x, setX) <- if y then "B" else the Double 0
--   Return $ (show x) ++ (show y)

e3 : StateBuilder ?e3h4 ?e3h5 String
e3 = do
  (y, setY) <- the Int 2
  (x, setX) <- "B"
  Return $ (show x) ++ (show y)

e3e = runNext e3 [] [20, "BB"]

e4 : StateBuilder (%search) ?e4h5 (HList ?e4h7)
e4 = do
  (x, setX) <- "C"
  (y, setY) <- the Int 3
  -- Return $ setX "CC"
  Return $ setY 33

e4e = runNext e4 [] (runInit e4)
