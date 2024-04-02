module Demo23

import Data.SortedMap.Dependent

import Language.Reflection
import Language.Reflection.TT
%language ElabReflection


interface Context (identity : String) where content : Type

data Cell : (identity : String) -> Type where
  MakeCell : {identity : String} -> {auto context : Context identity} -> content @{context} -> Cell identity

data Direction : Type where
  Row : Direction
  Col : Direction

data View : Type where
  Text : {default [] press : List ()} -> (content : String) -> View
  Input : (value : String) -> (change : String -> ()) -> View
  Flex : Direction -> (children : List View) -> View
  Provider : (identity : String) -> {auto context : Context identity} -> (value : content @{context}) -> (child : View) -> View
  Consumer : (identity : String) -> {auto context : Context identity} -> (initial : Maybe (content @{context})) -> (render : content @{context} -> View) -> View

data StateFacade : Type -> Type where
  MakeStateFacade : (identity : String) -> {auto context : Context identity} -> (initial : content @{context}) -> StateFacade (content @{context})

namespace OnContext
  export 
  (>>=) : (identity : String) -> {auto context : Context identity} -> (content @{context} -> View) -> View
  (>>=) identity next = Consumer identity Nothing next

namespace OnStateFacade
  export 
  (>>=) : StateFacade state -> ((state, state -> ()) -> View) -> View
  (>>=) (MakeStateFacade identity initial) next = Consumer identity (Just initial) (\state => next (state, \state => ()))

getIdentity : FC -> Elab String
getIdentity (MkFC (PhysicalIdrSrc (MkMI list)) (line, column) _) = pure "\{joinBy "/" list}.idr:\{show line}:\{show column}"
getIdentity fc = failAt fc "Could not generate identity" 

-- %macro
-- getTTImp : v -> Elab TTImp
-- getTTImp v = quote v

-- [minchia] Context "prova" where
--   content = String
--   initial = ""

-- u6 = (getTTImp minchia)

implementContextInterface : String -> Type -> Elab ()
implementContextInterface identity state = declare [
    IClaim EmptyFC MW Export [Hint True] (MkTy EmptyFC EmptyFC (UN (Basic "Context(fromString \"\{identity}\")")) (IApp EmptyFC (IVar EmptyFC (UN (Basic "Context"))) (IPrimVal EmptyFC (Str identity)))),
    IDef EmptyFC (UN (Basic "Context(fromString \"\{identity}\")")) [
      PatClause EmptyFC
        (IVar EmptyFC (UN (Basic "Context(fromString \"\{identity}\")")))
        (IApp EmptyFC (
            INamedApp EmptyFC
              (IVar EmptyFC (NS (MkNS ["Demo23"]) (UN (Basic "__mkContext"))))
              (UN (Basic "identity"))
              (IPrimVal EmptyFC (Str identity))
          ) !(quote state)
        )
    ]
  ]

%macro
createState : {state : Type} -> state -> Elab (StateFacade state)
createState {state} initial = do
  quotedInitial <- quote initial
  identity <- getIdentity (getFC quotedInitial)
  implementContextInterface identity state
  check `(MakeStateFacade ~(IPrimVal EmptyFC (Str identity)) ~(quotedInitial))

%macro
createContext : Type -> Elab String
createContext state = do
  identity <- getIdentity (getFC !(quote state))
  implementContextInterface identity state
  check (IPrimVal EmptyFC (Str identity))

unfold : SortedDMap String Cell -> View -> View
unfold contexts (Text content) = (Text content)
unfold contexts (Input value change) = (Input value change)
unfold contexts (Flex direction children) = (Flex direction (map (unfold contexts) children))
unfold contexts (Provider identity value child) = unfold (insert identity (MakeCell value) contexts) child
unfold contexts (Consumer identity initial render) with (lookup identity contexts)
  unfold contexts (Consumer identity initial render) | (Just (_ ** MakeCell content)) = render (believe_me content)
  unfold contexts (Consumer identity initial render) | Nothing = case initial of
    (Just initial) => render initial
    Nothing => (Text "Missing context \{identity}")
    

----

recordContext = createContext Int

Counter : {default 1 step : Int} -> View
Counter = do
  (count, setCount) <- createState (the Double 0)
  (count2, setCount2) <- createState (the Int 0)
  recordCount <- recordContext
  Text (show 0)



App = Flex Col [
  Text "hello world",
  Counter {step = 2},
  Provider recordContext 20 (
    Flex Col [
      Counter,
      Counter
    ]
  )
]