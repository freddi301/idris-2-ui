module Demo25.UI.View

import Demo25.UI.Style

import Data.SortedMap.Dependent
import Data.SortedMap
import Data.List.Extra

import public Data.Fin
import public Language.Reflection
import public Demo25.UI.Style

import Language.Reflection
import Language.Reflection.TT
%language ElabReflection

-- TODO reafator events to nicier dsl

public export
interface Context (identity : String) where content : Type

public export
data Cell : (identity : String) -> Type where
  MakeCell : {identity : String} -> {auto context : Context identity} -> content @{context} -> Cell identity

public export
data StateUpdate : Type where
  MakeStateUpdate : (path : List (String, String)) -> (identity : String) -> {auto context : Context identity} -> content @{context} -> StateUpdate

public export
data Direction : Type where
  Row : Direction
  Col : Direction

export
Eq Direction where
  (==) Row Row = True
  (==) Col Col = True
  (==) _ _ = False

public export
data View : Type where
  Text : {default defaultTextStyle style : TextStyle} -> {default [] press : List StateUpdate} -> (content : String) -> View
  Input : {default defaultInputStyle style : InputStyle} -> (value : String) -> (change : String -> List StateUpdate) -> View
  Flex : Direction -> {default defaultFlexStyle style : FlexStyle} -> {default [] press : List StateUpdate} -> (children : List View) -> View
  Provider : (identity : String) -> {auto context : Context identity} -> (value : content @{context}) -> (child : View) -> View
  Consumer : (identity : String) -> {auto context : Context identity} -> (child : content @{context} -> View) -> View
  State : (identity : String) -> {auto context : Context identity} -> (initial : content @{context}) -> (render : (List (String, String), content @{context}) -> View) -> View

public export
data StateFacade : Type -> Type where
  MakeStateFacade : (identity : String) -> {auto context : Context identity} -> (initial : content @{context}) -> StateFacade (content @{context})

namespace OnContext
  public export
  (>>=) : (identity : String) -> {auto context : Context identity} -> (content @{context} -> View) -> View
  (>>=) identity next = Consumer identity next

namespace OnStateFacade
  public export 
  (>>=) : StateFacade state -> ((state, state -> StateUpdate) -> View) -> View
  (>>=) (MakeStateFacade identity initial) next = State identity initial (\(path, state) => next (state, MakeStateUpdate path identity))

getIdentity : FC -> Elab String
getIdentity (MkFC (PhysicalIdrSrc (MkMI list)) (line, column) _) = pure "\{joinBy "/" list}.idr:\{show line}:\{show column}"
getIdentity fc = failAt fc "Could not generate identity" 

-- %macro
-- getTTImp : v -> Elab TTImp
-- getTTImp v = quote v

-- u6 = (getTTImp 4)

implementContextInterface : String -> Type -> Elab ()
implementContextInterface identity state = declare [
    IClaim EmptyFC MW Export [Hint True] (MkTy EmptyFC EmptyFC (UN (Basic "Context(fromString \"\{identity}\")")) (IApp EmptyFC (IVar EmptyFC (UN (Basic "Context"))) (IPrimVal EmptyFC (Str identity)))),
    IDef EmptyFC (UN (Basic "Context(fromString \"\{identity}\")")) [
      PatClause EmptyFC
        (IVar EmptyFC (UN (Basic "Context(fromString \"\{identity}\")")))
        (IApp EmptyFC (
            INamedApp EmptyFC
              (IVar EmptyFC (NS (MkNS ["View", "UI", "Demo25"]) (UN (Basic "__mkContext"))))
              (UN (Basic "identity"))
              (IPrimVal EmptyFC (Str identity))
          ) !(quote state)
        )
    ]
  ]

public export
%macro
useState : {state : Type} -> state -> Elab (StateFacade state)
useState {state} initial = do
  quotedInitial <- quote initial
  identity <- getIdentity (getFC quotedInitial)
  implementContextInterface identity state
  check `(MakeStateFacade ~(IPrimVal EmptyFC (Str identity)) ~(quotedInitial))

public export
%macro
createContext : Type -> Elab String
createContext state = do
  identity <- getIdentity (getFC !(quote state))
  implementContextInterface identity state
  check (IPrimVal EmptyFC (Str identity))

export
unfold :
  (contexts : SortedDMap String Cell) ->
  (states : SortedMap (List (String, String)) (identity : String ** Cell identity)) ->
  (path : List (String, String)) ->
  View ->
  View
unfold contexts states path view = rec view where
  rec : View -> View
  rec (Text { style, press } content) = (Text { style = style, press = press } content)
  rec (Input value change) = (Input value change)
  rec (Flex direction { style, press } children) = (Flex direction { style = style, press = press } (mapi (\index => \item => unfold contexts states (path ++ [("Flex", show index)]) item) children))
  rec (Provider identity value child) = unfold (insert identity (MakeCell value) contexts) states (path ++ [("Provider", identity)]) child
  rec (Consumer identity child) = case (lookup identity contexts) of
    (Just (_ ** MakeCell content)) => unfold contexts states (path ++ [("Consumer", identity)]) (child (believe_me content))
    Nothing => Text "Missing context \{identity}"
  rec (State identity initial render) = unfold contexts states (path ++ [("State", identity)]) (render (path, cont)) where
    cont = case (lookup path states) of
      (Just (existingIdentity ** MakeCell content)) => if existingIdentity == identity then (believe_me content) else initial
      Nothing => initial
    