module Demo5

-- https://reactnative.dev/docs/text-style-props
-- https://reactnative.dev/docs/view-style-props

mutual

  data Node : Type where
    Text :
      String ->
      Node
    TextInput :
      (value : String) ->
      (change : String -> Node) ->
      Node
    View :
      {default Nothing press : Maybe $ () -> Node} ->
      (children : List Node) ->
      Node
  
  Component : (stateType : Type) -> Type
  Component stateType = {state : stateType} -> {update: stateType -> Node} -> Node

---

Button :
  {label : String} ->
  {press : () -> Node} ->
  Node
Button = View {press = Just press} [Text label]

namespace Counter

  private
  record State where
    constructor Init
    count : Int

  render : {default 0 step : Int} -> Component Counter.State
  render = Button {label = show state.count, press = \_ => update $ { count := state.count + step} state}

namespace Todos

  record State where
    constructor Init
    inputText : String
    todos : List String

  render : Component Todos.State
  render = View [
    View [
      TextInput { value = state.inputText, change = \value => update $ { inputText := value } state },
      Button { label = "+", press = \event => update $ { todos := state.inputText :: state.todos, inputText := "" } state }
    ],
    View [ Text todo | todo <- state.todos ]
  ]  

namespace App

  record State where
    constructor Init
    counter1 : Counter.State
    counter2 : Counter.State
    todos1 : Todos.State

  render : Component App.State
  render = View [
    Counter.render {state = state.counter1, update = \substate => update $ { counter1 := substate } state},
    Counter.render {state = state.counter2, update = \substate => update $ { counter2 := substate } state},
    Todos.render {state = state.todos1, update = \substate => update $ { todos1 := substate } state}
  ]


app : Node
app = App.render {
  state = App.Init {
    counter1 = Init {count = 0},
    counter2 = Init {count = 0},
    todos1 = Init {inputText = "", todos = []}
  },
  update = \_ => Text "TODO"
}