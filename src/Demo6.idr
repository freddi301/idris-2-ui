module Demo6

-- https://reactnative.dev/docs/text-style-props
-- https://reactnative.dev/docs/view-style-props

mutual

  data Node : context -> Type where
    Text :
      String ->
      Node context
    TextInput :
      (value : String) ->
      (change : String -> context) ->
      Node context
    View :
      {default Nothing press : Maybe $ () -> context} ->
      (children : List (Node context)) ->
      Node context
  
  Stateless : Type -> Type
  Stateless context = context -> Node context

  Stateful : Type -> Type -> Type
  Stateful state context = context -> state -> (update: state -> context) -> Node context

---

Button :
  {label : String} ->
  {press : () -> context} ->
  Stateless context
Button context = View {press = Just press} [Text label] 

namespace Counter

  record State where
    constructor Init
    count : Int

  Counter :
    {default 0 step : Int} ->
    {default Nothing onStep : Maybe $ Int -> context} ->
    Stateful Counter.State context
  Counter context state update =
    Button context {
      label = show state.count,
      press = \_ => update $ { count := state.count + step} state
    }

namespace Todos

  record State where
    constructor Init
    inputText : String
    todos : List String

  Todos : Stateful Todos.State context
  Todos context state update =
    View [
      View [
        TextInput { value = state.inputText, change = \value => update $ { inputText := value } state },
        Button context { label = "+", press = \event => update $ { todos := state.inputText :: state.todos, inputText := "" } state }
      ],
      View [ Text todo | todo <- state.todos ]
    ]  

namespace App

  record State where
    constructor Init
    counter1 : Counter.State
    counter2 : Counter.State
    todos1 : Todos.State

  App : Stateful App.State context
  App context state update = View [
    Counter context state.counter1 (\substate => update $ { counter1 := substate } state),
    Counter context state.counter2 (\substate => update $ { counter2 := substate } state),
    Todos context state.todos1 (\substate => update $ { todos1 := substate } state)
  ]

initial : App.State
initial = Init {
  counter1 = Init {count = 0},
  counter2 = Init {count = 0},
  todos1 = Init {inputText = "", todos = []}
}
app : Node App.State
app = App initial initial (\_ => initial)