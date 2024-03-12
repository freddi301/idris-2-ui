module Demo14

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

unroll : Node -> Node
unroll (Text text) = Text text
unroll (Layout children) = Layout (map unroll children)
-- Layout(children.map((child, index) => (substate => 
-- Layout(children.map((c, i) => i === index ? render(substate) : render(state) ))
-- )))
unroll (Instance component) = render @{component} initial update where
  update state = render @{component} state update