module Demo21.Dsl

import public Data.List.Quantifiers

public export
data LayoutDirection : Type where
  Col : LayoutDirection
  Row : LayoutDirection

mutual

  public export
  data View : Type where
    DoNotUpdate : View
    Text : {default emptyBaseEventHandlers event : BaseEventHandlers } -> (content : String) -> View
    Input : (value : String) -> (change : String -> Lazy View) -> View
    Layout : LayoutDirection -> {default emptyBaseEventHandlers event : BaseEventHandlers } -> (children : List View) -> View

  export
  doNotUpdate : View
  doNotUpdate = DoNotUpdate

  public export
  interface BaseEventHandlers where
    constructor On
    press : Lazy View
    press = doNotUpdate

  [emptyBaseEventHandlers] BaseEventHandlers where

public export
data StateBuilder : List Type -> List Type -> Type -> Type -> Type where
  Return : value -> StateBuilder left [] setRet value
  (>>=) : value -> ((value, value -> setRet) -> StateBuilder (left ++ [value]) right setRet return) -> StateBuilder left (value :: right) setRet return

export
runInit : StateBuilder left right setRet return -> HList right
runInit (Return value) = Nil
runInit ((>>=) (initial) next) = initial :: (runInit (next (initial, \newValue => ?_)))

export
runNext : StateBuilder left right setRet return -> HList left -> HList right -> (HList (left ++ right) -> setRet) -> return
runNext (Return value) left Nil mapper = value
runNext ((>>=) (initial) next) left (value :: rest) mapper = runNext (next (value, setter)) (left ++ [value]) rest (believe_me mapper) where
  setter newValue = mapper $ left ++ [newValue] ++ rest

public export
Component : (shape : List Type) -> Type
Component shape = StateBuilder [] shape View View

export
Ref : (HList shape, HList shape -> View) -> Component shape -> View
Ref (state, update) component = runNext component [] state update

namespace StateBuilderProve

  e1 : StateBuilder [] [String, Int] (HList [String, Int]) String
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

  e3 : StateBuilder ?e3h4 ?e3h5 (HList ?e3h9) String
  e3 = do
    (y, setY) <- the Int 2
    (x, setX) <- "B"
    Return $ (show x) ++ (show y)

  e3e = runNext e3 [] [20, "BB"]

  e4 : StateBuilder ?e4h4 ?e4h5 (HList ?e4h9) (HList ?e4h7)
  e4 = do
    (x, setX) <- "C"
    (y, setY) <- the Int 3
    -- Return $ setX "CC"
    Return $ setY 33

  e4e = runNext e4 [] (runInit e4)

  record UseDoubleCounter setRet where
    constructor MakeUseDoubleCounter
    countX : Int
    countY : Int
    incX : setRet
    incY : setRet


  useDoubleCounter : {default 1 stepX : Int} -> {default 1 stepY : Int} -> StateBuilder left ?udch5 setRet (UseDoubleCounter setRet)
  useDoubleCounter = do
    (countX, setCountX) <- the Int 0
    (countY, setCountY) <- the Int 0
    Return $ MakeUseDoubleCounter {
      countX = countX,
      incX = setCountX $ countX + stepX,
      countY = countY,
      incY = setCountY $ countY + stepY
    }
    
  -- e5 : StateBuilder ?e5h4 ?e5h5 ?e5h9 ?e5h7
  -- e5 = do
  --   dc1 <- useDoubleCounter
  --   dc2 <- useDoubleCounter
  --   Return $ dc1.countX + dc2.countY