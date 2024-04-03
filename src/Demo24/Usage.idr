module Demo24.Usage

import Language.Reflection
import Demo24.View
import Demo24.Browser.View

--- Hello world

HelloWorldApp = Text "Hello World"

--- Naive Routing

data Route = HomeRoute | AboutRoute

HomeScreen = Text "Home"

AboutScreen = Text "About"

matchRoute : Route -> View
matchRoute HomeRoute = HomeScreen
matchRoute AboutRoute = AboutScreen

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

-- Component instances

Counter : {default 1 step : Int} -> View
Counter = do
  (count, setCount) <- useState (the Int 0)
  Flex Col [
    Text "\{show count}",
    Text {press = [setCount(count + step)]} "+\{show step}"
  ]

ComponentInstancesApp = Flex Col [
  Counter,
  Counter {step = 2}
]

--- render

main : IO ()
main = do
  root <- Root.create [HelloWorldApp, NaiveRoutingApp, ComponentInstancesApp]
  pure ()
