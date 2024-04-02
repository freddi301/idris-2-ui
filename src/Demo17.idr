module Demo17

mutual 

  data Tree : Type where
    Leaf : String -> Tree
    Branch : List Tree -> Tree
    Instance : Component state -> {default Nothing current : Maybe state} -> Tree

  interface Component state where
    initial : state
    render : state -> (update : state -> Tree) -> Tree

orElse : Maybe a -> a -> a
orElse Nothing def = def
orElse (Just val) def = val

append : List a -> a -> List a
append [] v = [v]
append (h :: t) v = h :: (append t v)

e1 : Tree
e1 = Leaf "Hello"

[Counter] { default 1 step : Int } -> Component Int where
  initial = 0
  render state update = Leaf (show state)

e2 : Tree
e2 = Instance Counter

e3 : Tree
e3 = Branch [
  Instance Counter,
  Instance Counter
]

[App] Component String where
  initial = ""
  render state udpate = Branch [
    Leaf state,
    Instance $ Counter { step = 2 },
    Instance $ Counter
  ]

renderTree : (Tree -> Tree) -> Tree -> Tree
renderTree parent (Leaf content) = (Leaf content)
renderTree parent (Branch children) = (Branch (renderBranch [] children)) where
  renderBranch : List Tree -> List Tree -> List Tree
  renderBranch prev [] = prev
  renderBranch prev (h :: t) = renderBranch (append prev (renderTree branchParent h)) t where
    branchParent child = parent (Branch ((append prev child) ++ (renderBranch (append prev child) t)))
renderTree parent (Instance component {current}) = render @{component} (orElse current (initial @{component})) update where
  update state = parent $ renderTree parent (Instance component {current = Just state})
