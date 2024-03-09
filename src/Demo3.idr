module Demo3

data ViewFlexDirection : Type where
  Row : ViewFlexDirection
  Column : ViewFlexDirection

data Color : Type where
  RGB : Int -> Int -> Int -> Color

data ViewStyle : Type where
  Style : 
    {default Column flexDirection : ViewFlexDirection} ->
    {default (RGB 0 0 0) borderColor : Color} ->
    {default 0 borderWidth : Int} ->
    ViewStyle

data ViewNode : globalState -> Type where
  Text : String -> ViewNode globalState
  TextInput : {value: String} -> {onChange: String -> globalState} -> ViewNode globalState
  View :
    {default Style style : ViewStyle} ->
    {default Nothing onPress : Maybe (() -> globalState)} ->
    List (ViewNode globalState) ->
    ViewNode globalState
  ViewList : List (ViewNode globalState) -> ViewNode globalState

--

Stateful : (stateType : Type) -> {globalStateType : Type} -> Type
Stateful stateType = {state : stateType} -> {update: stateType -> globalStateType} -> ViewNode globalStateType

record CounterState where
  constructor MkCounterState
  count : Int

Counter : {default 1 step : Int} -> Stateful CounterState
Counter = View {onPress = Just $ \_ => update $ { count := state.count + step } state} [Text (show state.count)]

record TodosState where
  constructor MkTodosState 
  inputText : String
  todos : List String

Todos : Stateful TodosState
Todos = View [
    Text "Todo app demo",
    View {style = Style {flexDirection = Row}} [
      TextInput {value = state.inputText} {onChange = \text => update $ {inputText := text} state},
      View {onPress = Just (\_ => update $ {todos := state.inputText :: state.todos} state)} {style = Style {borderWidth = 1}} [Text "Add"]
    ],
    ViewList [View [Text todo] | todo <- state.todos]
  ]

record AppState where
  constructor MkAppState
  counter1 : CounterState
  counter2 : CounterState
  todos1 : TodosState

App : Stateful AppState
App = View [
  Counter {state = state.counter1} {update = \substate => update $ { counter1 := substate } state},
  Counter {step = 2} {state = state.counter2} {update = \substate => update $ { counter2 := substate } state},
  Todos {state = state.todos1} {update = \substate => update $ { todos1 := substate } state}
]

example : ViewNode AppState
example = App
  {state = MkAppState
    {counter1 = MkCounterState {count = 0}}
    {counter2 = MkCounterState {count = 0}}
    {todos1 = MkTodosState {inputText = ""} {todos = []}}
  }
  {update = id}