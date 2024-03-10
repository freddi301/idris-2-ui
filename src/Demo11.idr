module Demo11

mutual 

  data Child : Type where
    MkChild : Shape width height -> Child

  data Shape : (width : Nat) -> (height : Nat) -> Type where
    Rect : (width : Nat) -> (height : Nat) -> Shape width height
    Layout : (children : List Child) -> Shape (sumWidth children) (sumHeight children)

  getHeight : Shape width height -> Nat
  getHeight (Rect width height) = height 
  getHeight (Layout children) = sumHeight children 

  sumHeight : List Child -> Nat
  sumHeight [] = 0 
  sumHeight ((MkChild head) :: tail) = (getHeight head) + (sumHeight tail)

  getWidth : Shape width height -> Nat
  getWidth (Rect width height) = height 
  getWidth (Layout children) = sumWidth children 

  sumWidth : List Child -> Nat
  sumWidth [] = 0
  sumWidth ((MkChild head) :: tail) = (getWidth head) + (sumWidth tail)


e1 = Layout [MkChild $ Rect 10 20, MkChild $ Rect 20 30]

e2 = getHeight e1