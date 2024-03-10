module Demo12

data Shape : (width : Nat) -> (height : Nat) -> Type where
  Rect : (width : Nat) -> (height : Nat) -> Shape width height

data LessEqual : Nat -> Nat -> Type where
  ProveLessEqual : {a : Nat} -> {b : Nat} -> LessEqual a (a + b)

%hint
lessy : {x: Nat} -> {y : Nat} -> LessEqual x y
lessy = ?hole -- ProveLessEqual {a = x} {b = y}

renderOnScreen :
  (screenWidth : Nat) ->
  (screenHeight : Nat) ->
  {shapeWidth : Nat} -> {shapeHeight : Nat} ->
  (shape : Shape shapeWidth shapeHeight) ->
  {auto a: LessEqual shapeWidth screenWidth} ->
  {auto b: LessEqual shapeHeight screenHeight} ->
  ()
renderOnScreen _ _ _ = ()

e1 : ()
e1 = renderOnScreen 100 100 (Rect 50 50)