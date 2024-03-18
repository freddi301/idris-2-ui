module Demo21.Usage

import Demo21.Dsl
import Demo21.Browser

import Data.List 

map : (l : List a) -> ((i : (a, Nat)) -> {0 inBounds : InBounds (snd i) l} -> b) -> {default Z ci : Nat} -> List b
map Nil m = Nil
map (h :: t) m = (m (h, ci) {inBounds = ?_hib}) :: (map t (believe_me m) {ci = S ci})

Counter : {default 1 step : Int} -> Component ?_1
Counter = do
  (count, setCount) <- the Int 0
  Return $ Layout Row [
    Text (show count),
    Text {event = On { press = setCount $ count + step }} ("+ \{show step}")
  ]

CounterExample : Component ?_2
CounterExample = do
  counter1Ref <- runInit $ Counter
  counter2Ref <- runInit $ Counter {step = 2}
  Return $ Layout Col [
    Text "Counter example",
    Ref counter1Ref $ Counter,
    Ref counter2Ref $ Counter {step = 2}
  ]

Todos : Component ?_3
Todos = do
  (inputText, setInputText) <- ""
  (todos, setTodos) <- the (List String) []
  Return $ Layout Col [
    Text "Todos",
    Layout Row [
      Input {value = inputText, change = \inputText => setInputText inputText},
      Text {event = On { press = setTodos $ inputText :: todos }} "Add"
    ],
    Layout Col $ map todos $ \(todo, todoIndex) =>
      Layout Row [Text todo, Text {event = On {press = setTodos $ deleteAt todoIndex todos}} "x"]
  ]

App : Component ?_4
App = do
  (route, setRoute) <- Text ""
  counters <- runInit $ CounterExample
  todos <- runInit $ Todos
  Return $ Layout Col [
    Ref counters $ CounterExample,
    Ref todos $ Todos
  ]


main : IO ()
main = renderRoot $ App
