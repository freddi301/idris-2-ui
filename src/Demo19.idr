module Demo19

import Data.List

-- -- data Mico : Type -> Type where
-- --   Con : v -> Mico v


-- -- (>>=) : Mico a -> (a -> Mico b) -> Mico b
-- -- (>>=) (Con v) f = f v

-- -- (>>) : Mico () -> Mico b -> Mico b
-- -- (>>) _ r = r

-- -- e1 : Mico Int
-- -- e1 = do
-- --   a <- Con True
-- --   b <- Con "hello"
-- --   Con (the Int 5)



-- data Tuple : List Type -> Type where
--   Nil : Tuple []
--   (::) : h -> Tuple t -> Tuple (h :: t)
    

-- e1 : List Type
-- e1 = [Bool, String, Int]


-- e2 : Tuple [Bool, Int, String]
-- e2 = [True, 42, "Hello"]


-- infix 0 :::

-- (:::) : String -> Type -> (String, Type)
-- (:::) key value = (key, value)

-- e3 : List (String, Type)
-- e3 = ["x" ::: Int, "y" ::: Double]

-- has : Eq value => value -> List value -> Bool
-- has _ Nil = False
-- has value (head :: tail) = (value == head) || (has value tail)

-- addUnique : Eq v => (value : v) -> (list : List v) -> has value list = False => List v
-- addUnique value list = value :: list

-- e1 = addUnique "a" []

-- e2 = addUnique "b" ["a"]

-- e3 = addUnique "c" ["a", "b"]

-- mutual

--   data UniqueList : Type -> Type where
--     Nil : UniqueList itemType
--     (::) : (headValue : itemType) -> (tailValue : UniqueList itemType) -> {auto eq : Eq itemType} -> myHas eq headValue tailValue = False => UniqueList itemType

--   myHas : Eq itemType -> itemType -> UniqueList itemType -> Bool
--   myHas eq itemValue Nil = False
--   myHas eq itemValue ((::) headValue tailValue) = ((==) @{eq} itemValue headValue) || (myHas eq itemValue tailValue)

-- e1 : UniqueList String 
-- e1 = []

-- e2 : UniqueList String 
-- e2 = ["a"]

-- e3 : UniqueList String
-- e3 = ["a", "b"]

-- e4 : UniqueList String
-- e4 = ["a", "b", "c"]

-- data HeterogenousList : List Type -> Type where
--   Nil : HeterogenousList []
--   (::) : item -> HeterogenousList tail -> HeterogenousList (item :: tail)

data StateBuilder : (continuable : Bool) -> (shape : List Type) -> (onThen : Type) -> (return : Type) -> Type where
  Init : value -> StateBuilder True types value ()
  Continue : StateBuilder True x a j -> ((a, a) -> StateBuilder cc y b k) -> StateBuilder cc (x ++ y) b k
  Return : value -> StateBuilder False types () value

useState : state -> {auto types : List Type} -> StateBuilder True (snoc types state) state ()
useState initial = Init initial

return : value -> {auto types : List Type} -> StateBuilder False types () value
return value = Return value

(>>=) : StateBuilder True x a j -> ((a, a) -> StateBuilder cc y b k) -> StateBuilder cc (x ++ y) b k
(>>=) prev next = Continue prev next

e1 : StateBuilder False [String, Bool] () String
e1 = do
  (x, setX) <- useState "A"
  (y, setY) <- useState False
  return $ (show x) ++ (show y)

e3 : StateBuilder False ?_k () String
e3 = do
  (x, setX) <- useState "B"
  (y, setY) <- useState True
  return $ (show x) ++ (show y)

runInit : StateBuilder False types () return -> return
runInit (Return value) = value
runInit (Continue (Init initial) next) = runInit $ next (initial, initial)
runInit (Continue (Continue x f) next) = ?___O_1
