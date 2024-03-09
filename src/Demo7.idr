module Demo7

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
    Instance : (component: state -> (udpate : state -> Node) -> Node) -> Node
  
  Component : Type -> Type
  Component state = state -> (udpate : state -> Node) -> Node

---

Button :
  {label : String} ->
  {press : () -> Node} ->
  Node
Button = View {press = Just press} [Text label] 


record CounterState where
  constructor MkCounterState
  count : Int

Counter :
  {default 0 step : Int} ->
  Component CounterState
Counter state update =
  Button {
    label = show state.count,
    press = \_ => update $ { count := state.count + step} state
  }

record TodosState where
  constructor MkTodosState
  inputText : String
  todos : List String

Todos : Component TodosState
Todos state update =
  View [
    View [
      TextInput { value = state.inputText, change = \value => update $ { inputText := value } state },
      Button { label = "+", press = \event => update $ { todos := state.inputText :: state.todos, inputText := "" } state }
    ],
    View [ Text todo | todo <- state.todos ]
  ]  

App : Node
App = View [
  Instance Counter,
  Instance (Counter {step = 2}),
  Instance Todos
]
