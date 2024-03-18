module Demo18.Usage

import Demo18.Dsl
import Demo18.Browser

[Counter] {default 1 step : Int} -> Component Int where
  initial = 0
  render state update = Layout [
    Text (show state),
    Text {on = event { press = update $ state + step}} ("+ " ++ (show step))
  ]


CounterExamplePage : DataView
CounterExamplePage = Layout [
  Text "Counter example",
  Instance Counter,
  Instance Counter
]

data Route : Type where
  Home : Route
  About : Route
  CounterExample : Route


Router : Route -> DataView
Router Home = Text "Home Page"
Router About = Text "About Page"
Router CounterExample = CounterExamplePage

mutual 

  Link : (to : Route) -> (label : String) -> DataView
  Link to label = Text { on = event { press = App "" to } } label

  App : String -> Route -> DataView
  App text route = Layout [
    Input {value = text, change = \value => App value route},
    Text (text ++ text),
    Layout [
      Link Home "Home",
      Link About "About",
      Link CounterExample "Counter Example"
    ],
    Router route
  ]


app : DataView
app = App "" CounterExample

main : IO ()
main = render CounterExamplePage
