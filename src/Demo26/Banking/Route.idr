module Demo26.Banking.Route

import Demo26.UI.View

import Demo26.Banking.Domain

public export
data Route : Type where
  Settings : Route
  Accounts : Route
  Transactions : Route
  Account : AccountId -> Route

export
Eq Route where
  (==) Settings Settings = True
  (==) Accounts Accounts = True
  (==) Transactions Transactions = True
  (==) (Account accountId_x) (Account accountId_y) = accountId_x == accountId_y
  (==) _ _ = False

RouteContext = createContext Route
SetRouteContext = createContext (Route -> StateUpdate)

export
provideRoute : (Route -> View) -> View
provideRoute child = do
  (route, setRoute) <- useState $ the Route Accounts
  Provider RouteContext route $ Provider SetRouteContext setRoute $
  child route

export
useRoute : Exposed Route
useRoute = Expose $ \expose => do
  route <- RouteContext
  expose route

export
useNavigate : Exposed (Route -> StateUpdate)
useNavigate = Expose $ \expose => do
  setRoute <- SetRouteContext
  expose setRoute
