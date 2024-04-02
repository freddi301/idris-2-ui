module Demo22
import Language.Reflection
import Language.Reflection.TT
import Data.Fin
import Data.String
import Data.SortedMap
import Data.Vect

%language ElabReflection


data Color : Type where
  RGBA : Int8 -> Int8 -> Int8 -> Double -> Color

namespace Color
  export
  rgba : (red : Int8) ->  (green : Int8) -> (blue : Int8) -> (alpha : Double) -> Color
  rgba = RGBA
  export
  hex : (colorHexString : String) -> { auto l : strLength colorHexString = 6 } -> Color
  hex string = RGBA 0 0 0 1 -- TODO

record TextStyle where
  constructor MakeTextStyle
  color : Color

DefaultTextStyle : TextStyle
DefaultTextStyle = MakeTextStyle { color = rgba 0 0 0 1 }

record Margin where
  constructor MakeMargin
  top : Int
  right : Int
  bottom : Int
  left : Int

DefaultMargin : Margin
DefaultMargin = MakeMargin { top = 0, right = 0, bottom = 0, left = 0 }

record Padding where
  constructor MakePadding
  top : Int
  right : Int
  bottom : Int
  left : Int

DefaultPadding : Padding
DefaultPadding = MakePadding { top = 0, right = 0, bottom = 0, left = 0 }

record LayoutStyle where
  constructor MakeLayoutStyle
  margin : Margin
  padding : Padding

DefaultLayoutStyle : LayoutStyle
DefaultLayoutStyle = MakeLayoutStyle { margin = DefaultMargin, padding = DefaultPadding }

onSides : Type -> Type
onSides t =
  { default 0 all : Int } -> 
  { default all horizontal : Int } -> 
  { default all vertical : Int } -> 
  { default vertical top : Int } ->
  { default horizontal right : Int } ->
  { default vertical bottom : Int } ->
  { default horizontal left : Int } ->
  t

namespace Margin
  export
  s : onSides Margin
  s { top, right, bottom, left } = MakeMargin { top = top, right = right, bottom = bottom, left = left } 

namespace Padding
  export
  s : onSides Padding
  s { top, right, bottom, left } = MakePadding { top = top, right = right, bottom = bottom, left = left }

namespace LayoutStyle
  export
  s :
    { default DefaultMargin margin : Margin } ->
    { default DefaultPadding padding : Padding } ->
    LayoutStyle
  s = MakeLayoutStyle { margin = margin, padding = padding } 

namespace TextStyle
  export
  s : { default (rgba 0 0 0 1) color : Color } -> TextStyle
  s = MakeTextStyle { color = color }

record LayoutEvents (context : Type) where
  constructor MakeLayoutEvents
  press : Lazy context

DefaultLayoutEvents : (context : Type) -> LayoutEvents context
DefaultLayoutEvents context = MakeLayoutEvents { context = context } { press = ?h34 }

namespace LayoutEvents
  export
  on : { default ?h2 press : Lazy context } -> LayoutEvents context
  on { press } = MakeLayoutEvents { press = press }

mutual

  data View_ : (context : Type) -> Type where
    Empty : View_ context
    Text : { default DefaultTextStyle style : TextStyle } -> (content : String) -> View_ context
    Input : { value : String } -> { change : String -> Lazy context } -> View_ context
    Layout : { default (DefaultLayoutEvents ?h1) event : LayoutEvents context } -> { default DefaultLayoutStyle style : LayoutStyle } -> (children : List (View_ context)) -> View_ context
    Instance : (component : Component state context) -> View_ context
    Local : (state, state -> (update : state -> context) -> View_ context) -> View_ context

  interface Component state context where
    initial : state
    render : state -> (update: state -> context) -> View_ context

  Stateless : Type
  Stateless = { context : Type } -> View_ context

---

Hello : Stateless
Hello = Text "Ciao"

record CounterState where
  constructor MakeCounterState
  count : Int

Counter : { default 1 step : Int } -> Stateless
Counter = Instance component where
  [component] Component CounterState context where
    initial = MakeCounterState { count = 0 }
    render state update = view where
      inc = update $ { count := state.count + 1 } state
      view = Layout { event = on{ press = inc } } [
        Text "\{show state.count}",
        Text { style = s{ color = hex "ffffff" } } "+ \{show step}"
      ]

CounterExample : Stateless
CounterExample = Layout {
  style = s{
    margin = s{ top = 2, left = 20 },
    padding = s{ horizontal = 15 }
  }
} [
  Counter,
  Counter {step = 3}
]
    

App : Stateless
App = Layout [
  CounterExample,
  CounterExample,
  CounterExample,
  CounterExample,
  CounterExample,
  CounterExample,
  CounterExample,
  CounterExample
]

---


interface Signature (identity : String) where state : Type

-- data Cell : (identity : String) -> {auto signature : Signature identity} -> Type where
--   Set : {identity : String} -> {auto signature : Signature identity} -> state @{signature} -> Cell identity
--   Unset : {identity : String} -> {auto signature : Signature identity} -> Cell identity

-- extract :
--   (nextIdentity : String) -> {auto nextSignature : Signature nextIdentity} -> state @{nextSignature} ->
--   {prevIdentity : String} -> {auto prevSignature : Signature prevIdentity} -> Cell prevIdentity ->
--   state @{nextSignature}
-- extract nextIdentity nextState Unset = nextState
-- extract nextIdentity nextState (Set prevState) = if prevIdentity == nextIdentity then (believe_me prevState) else nextState

-- Signature "x" where state = Int
-- Signature "y" where state = Int
-- Signature "z" where state = String

-- e1 = extract "x" 8 (Set { identity = "x" } 4)
-- e2 = extract "x" 8 (Unset { identity = "x" })
-- e3 = extract "y" 8 (Set { identity = "x" } 4)


namespace Other

  data TreeNode : Type where
    Leaf : String -> TreeNode
    Branch : List TreeNode -> TreeNode
    Instance : {state : Type} -> (initial : state) -> (render : state -> (update : state -> TreeNode) -> TreeNode) -> TreeNode

  renderTreeNode : (root : TreeNode) -> (store : SortedMap (List Int) (t : Type ** t)) -> (key : List Int) -> TreeNode -> TreeNode
  renderTreeNode root store key (Leaf content) = Leaf content
  renderTreeNode root store key (Branch children) = Branch (perChildren 0 children) where
    perChildren : Int -> List TreeNode -> List TreeNode
    perChildren index [] = []
    perChildren index (h :: t) = (renderTreeNode root store (key ++ [index]) h) :: (perChildren (index + 1) t)
  renderTreeNode root store key (Instance {state} initial render) = renderTreeNode root store key (render (initialize (lookup key store)) (\next => renderTreeNode root (insert key (state ** next) store) key root)) where
    initialize : Maybe (t : Type ** t) -> state
    initialize (Just (t ** current)) = initial -- if t == state then current else initial
    initialize Nothing = initial

 

%macro
myMacro : Elab ()
myMacro = do
  declare [
    IData EmptyFC DefaultedValue Nothing $ MkData EmptyFC (UN $ Basic "MyDataType") (Just $ IType EmptyFC) [] [
      MkTy EmptyFC EmptyFC (UN $ Basic $ "MyDataTypeX") (IVar EmptyFC (UN $ Basic "MyDataType")), 
      MkTy EmptyFC EmptyFC (UN $ Basic $ "MyDataTypeY") (IVar EmptyFC (UN $ Basic "MyDataType"))
    ]
  ]
  -- check (IVar EmptyFC (UN $ Basic name))

u = myMacro

myMacro2 : e -> Elab (TTImp)
myMacro2 e = quote e


u2_ = (MakeCounterState 6)
u2 = myMacro2 u2_.count