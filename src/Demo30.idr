module Demo30

import Data.SortedMap

data Markup : (clientState : Type) -> (serverState : Type) -> Type where
  Text : String -> Markup clientState serverState
  Block : List (Markup clientState serverState) -> Markup clientState serverState
  Action : String -> Maybe clientState -> Maybe serverState -> Markup clientState serverState

interface Application (clientState : Type) (serverState : Type) where
  render : clientState -> serverState -> Markup clientState serverState

data MyRoute : Type where
  Index : (message : String) -> MyRoute
  Person : MyRoute
  Product : MyRoute

record MyClientState where
  toBe : Int
  route : MyRoute

record MyServerState where
  actual : Int
  persons : SortedMap Int String
  products : SortedMap Int String

Application MyClientState MyServerState where
  render clientState serverState = case clientState.route of
    (Index message) =>
      Block [
        Text "Message: \{message}",
        Text "Actual: \{show serverState.actual}",
        Text "To be: \{show clientState.toBe}",
        Action "Inc" (Just $ { toBe := clientState.toBe + 1 } clientState) Nothing,
        Action "Dec" (Just $ { toBe := clientState.toBe + 1 } clientState) Nothing,
        Action "Confirm" (Just $ { route := Index {message = "Ok"} } clientState) (Just $ { actual := clientState.toBe } serverState),
        Action "Person" (Just $ { route := Person } clientState) Nothing,
        Action "Product" (Just $ { route := Product } clientState) Nothing
      ]
    Person =>
      let id = serverState.actual in
      Block [
        Text "Person: \{show id}",
        Text "Name: \{show $ lookup id serverState.persons}"
      ]
    Product =>
      let id = clientState.toBe in
      Block [
        Text "Product: \{show id}",
        Text "Name: \{show $ lookup id serverState.products}"
      ]
