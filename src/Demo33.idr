module Demo33

interface MyNot where constructor MakeMyNot myNot : Bool -> Bool
interface MyAnd where myAnd : Bool -> Bool -> Bool

u1 : MyNot => MyAnd => Bool -> Bool -> Bool
u1 x y = myNot (myAnd x y)

[MyNot1] MyNot where
  myNot True = False
  myNot False = True
 
u2 : MyAnd => Bool
u2 = let mnq = MyNot1 in u1 True False
