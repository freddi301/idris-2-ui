module Demo29

import Data.SortedMap

data Markup : (request : Type) -> Type where
  Text : String -> Markup request
  Block : List (Markup request) -> Markup request
  Action : request -> Markup request -> Markup request

interface Application (state : Type) (request : Type) where
  handle : state -> request -> (state, Markup request)

record MyState where
  toBe : Int
  actual : Int
  persons : SortedMap Int String
  products : SortedMap Int String

data MyRequest : Type where
  Inc : MyRequest
  Dec : MyRequest
  Index : MyRequest
  Confirm : MyRequest
  Person : Int -> MyRequest
  Product : Int -> MyRequest

renderIndex : MyState -> Markup MyRequest
renderIndex state =
  Block [
    Text "Actual: \{show state.actual}",
    Text "To be: \{show state.toBe}",
    Action Inc $ Text "Inc",
    Action Dec $ Text "Dec",
    Action Confirm $ Text "Confirm",
    Action (Person state.actual) $ Text "Person",
    Action (Product state.toBe) $ Text "Product"
  ]

renderPerson : MyState -> Int -> Markup MyRequest
renderPerson state id =
  Block [
    Text "Person: \{show id}",
    Text "Name: \{show $ lookup id state.persons}"
  ]

renderProduct : MyState -> Int -> Markup MyRequest
renderProduct state id =
  Block [
    Text "Product: \{show id}",
    Text "Name: \{show $ lookup id state.products}"
  ]

Application MyState MyRequest where
  handle state Inc =
    let state = { toBe := state.toBe + 1 } state in
    (state, renderIndex state)
  handle state Dec =
    let state = { toBe := state.toBe - 1 } state in
    (state, renderIndex state)
  handle state Index =
    (state, renderIndex state)
  handle state Confirm =
    let state = { actual := state.toBe } state in
    (state, renderIndex state)
  handle state (Person id) =
    (state, renderPerson state id)
  handle state (Product id) =
    (state, renderProduct state id)
