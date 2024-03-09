module Demo4

import Data.Fin

namespace Fremework
  
  namespace Style

    public export
    data Color : Type where
      RGB : Fin 256 -> Fin 256 -> Fin 256 -> Color

    namespace Text

      public export
      data Style : Type where
        MkStyle :
          {default (RGB 255 255 255) color : Color} -> 
          Style

    namespace View

      namespace Flex

        public export
        data Direction : Type where
          Row : Direction
          Column : Direction

      public export
      data Style : Type where
        MkStyle : 
          {default Column flexDirection : Direction} ->
          {default (RGB 0 0 0) borderColor : Color} ->
          {default 0 borderWidth : Int} ->
          Style

  public export
  data Node : state -> Type where
    Text :
      {default MkStyle style : Text.Style} ->
      String ->
      Node state
    TextInput : {value: String} -> {onChange: String -> state} -> Node state
    View :
      {default MkStyle style : View.Style} ->
      {default Nothing onPress : Maybe (() -> state)} ->
      (children : List (Node state)) ->
      Node state
    ViewList : List (Node state) -> Node state

  public export
  Stateful : (stateType : Type) -> {globalStateType : Type} -> Type
  Stateful stateType = {state : stateType} -> {update: stateType -> globalStateType} -> Node globalStateType

---

namespace Counter
  record State where
    constructor Make
    count : Int
  Counter : {default 1 step : Int} -> Stateful Counter.State
  Counter =
    View {
      onPress = Just $ \_ => update $ { count := state.count + step } state
    } [
      Text (show state.count)
    ]

namespace Todos
  record State where
    constructor Make 
    inputText : String
    todos : List String
  Todos : Stateful Todos.State
  Todos = 
    View [
      Text "Todo app demo",
      View {
        style = MkStyle {flexDirection = Row}
      } [
        TextInput {
          value = state.inputText,
          onChange = \text => update $ {inputText := text} state
        },
        View {
          style = MkStyle {borderWidth = 1},
          onPress = Just (\_ => update $ {todos := state.inputText :: state.todos} state)
        } [
          Text {style = MkStyle {color = RGB 0 0 255}} "Add"
        ]
      ],
      View [Text todo | todo <- state.todos]
    ]

namespace App

  record State where
    constructor Make
    counter1 : Counter.State
    counter2 : Counter.State
    todos1 : Todos.State

  App : Stateful App.State
  App =
    View [
      Counter {state = state.counter1, update = \substate => update $ { counter1 := substate } state},
      Counter {step = 2} {state = state.counter2, update = \substate => update $ { counter2 := substate } state},
      Todos {state = state.todos1, update = \substate => update $ { todos1 := substate } state}
    ]

example : Node App.State
example = App {
  state = Make {
    counter1 = Make {count = 0},
    counter2 = Make {count = 0},
    todos1 = Make {inputText = ""} {todos = []}
  },
  update = id
}
