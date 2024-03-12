module Demo13

mutual

  data Node : Type where
    Text : String -> Node
    Layout : List Node -> Node
    Instance : Component state -> Node

  interface Component state where
    initial : state
    render : state -> (update: state -> Node) -> Node

---

[Counter] {default 0 step : Int} -> Component Int where
  initial = step
  render state udpate = Text $ show state

e1 = Counter {step = 4}
e2 = initial @{Counter {step = 4}}

[MyApp] Component () where
  initial = ()
  render state update = Layout [
    Instance $ Counter,
    Instance $ Counter {step = 2}
  ]

e3 = MyApp
e4 = render @{MyApp} () (\_ => Text "")