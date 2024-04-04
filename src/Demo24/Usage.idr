module Demo24.Usage

import Language.Reflection
import Demo24.View
import Demo24.Browser.View

--- Hello world

HelloWorldApp = Text "Hello World"

--- Naive Routing

namespace NaiveRouting

  data Route = HomeRoute | AboutRoute

  HomeScreen = Text "Home"

  AboutScreen = Text "About"

  matchRoute : Route -> View
  matchRoute HomeRoute = HomeScreen
  matchRoute AboutRoute = AboutScreen

  export
  NaiveRoutingApp : View
  NaiveRoutingApp = do
    (currentRoute, setCurrentRoute) <- useState HomeRoute
    Flex Col [
      Flex Row [
        Text {press = [setCurrentRoute HomeRoute]} "Home",
        Text {press = [setCurrentRoute AboutRoute]} "About"
      ],
      matchRoute currentRoute
    ]

--- Component instances

Counter : {default 1 step : Int} -> View
Counter = do
  (count, setCount) <- useState (the Int 0)
  Flex Row [
    Text "\{show count}",
    Text {press = [setCount(count + step)]} "+\{show step}"
  ]

ComponentInstancesApp = Flex Col [
  Counter,
  Counter {step = 2}
]

--- Simple todos

SimpleTodosApp = do
  (text, setText) <- useState (the String "")
  (todos, setTodos) <- useState (the (List String) [])
  let addTodo = [
    setTodos (text :: todos),
    setText ""
  ]
  Flex Col [
    Flex Row [
      Input text (\text => [setText text]),
      Text { press = addTodo } "add"
    ],
    Flex Col $ (flip map) todos $ \todo => Text todo
  ]

--- Routing

namespace Routing

  interface Route where view : View

  navigateContext = createContext (Route -> List StateUpdate)
  routeContext = createContext Route

  Link : (to : Route) -> (content : String) -> View
  Link to content = do
    navigate <- navigateContext
    Text {press = navigate to} content

  Router : (initial : Route) -> (render: (outlet : View) -> View) -> View
  Router initial render = do
    (currentRoute, setCurrentRoute) <- useState initial
    let navigate = \to => [setCurrentRoute to]
    let contextProviders = (Provider navigateContext navigate) . (Provider routeContext currentRoute)
    contextProviders (render (view @{currentRoute}))

  ---

  [Home] Route where
    view = Text "HOME"

  [About] Route where
    view = Text "ABOUT"

  [Product] {id : String} -> Route where
    view = Text "PRODUCT \{id}"

  [Counters] Route where
    view = Flex Col [
      Counter,
      Counter {step = 3}
    ]

  export
  RoutingApp : View
  RoutingApp = Router Home $ \outlet =>
    Flex Col [
      Text "RoutingApp",
      Flex Row [
        Link Home "Home",
        Link About "About",
        Link (Product {id = "1"}) "Product (1)",
        Link (Product {id = "2"}) "Product (2)",
        Link Counters "Counters"
      ],
      outlet
    ]



--- render

main : IO ()
main = do
  root <- Root.create
  root.render [
    HelloWorldApp, 
    NaiveRoutingApp, 
    ComponentInstancesApp, 
    SimpleTodosApp,
    RoutingApp
  ]
