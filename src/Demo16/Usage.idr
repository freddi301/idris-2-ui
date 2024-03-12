module Demo16.Usage

import Demo16.Dsl
import Demo16.Browser

Counter : {default 1 step : Int} -> {count : Int} -> {setCount : Int -> View} -> View
Counter = Layout [
  Text (show count),
  Text {on = event { press = setCount $ count + step}} ("+ " ++ (show step))
]

CounterExamplePage : View
CounterExamplePage = Layout [

]

data Route : Type where
  Home : Route
  About : Route
  CounterExample : Route


Router : Route -> View
Router Home = Text "Home Page"
Router About = Text "About Page"
Router CounterExample = CounterExamplePage

mutual 

  Link : (to : Route) -> (label : String) -> View
  Link to label = Text { on = event { press = App "" to } } label

  App : String -> Route -> View
  App text route = Layout [
    Data.Input {value = text, change = \value => App value route},
    Text (text ++ text),
    Layout [
      Link Home "Home",
      Link About "About",
      Link CounterExample "Counter Example"
    ],
    Router route
  ]


app : View
app = App "" Home

main : IO ()
main = render app
