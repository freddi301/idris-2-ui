module Demo2
import Language.Reflection
import Deriving.Show

%language ElabReflection

data ViewFlexDirection : Type where
  Row : ViewFlexDirection
  Column : ViewFlexDirection

data Color : Type where
  RGB : Int -> Int -> Int -> Color

data ViewStyle : Type where
  Style : 
    {default Column flexDirection : ViewFlexDirection} ->
    {default (RGB 0 0 0) borderColor : Color} ->
    {default 0 borderWidth : Int} ->
    ViewStyle

mutual
  data ViewNode : Type where
    Text : String -> ViewNode
    TextInput : {value: String} -> {onChange: String -> ViewNode} -> ViewNode
    View :
      {default Style style : ViewStyle} ->
      {default emptyCallback onPress : () -> ViewNode} ->
      List ViewNode ->
      ViewNode
    ViewList : List ViewNode -> ViewNode 
  emptyCallback : () -> ViewNode
  emptyCallback _ = Text "TODO"


%hint
ViewFlexDirectionShow : Show ViewFlexDirection
ViewFlexDirectionShow = %runElab derive

%hint
ViewStyleShow : Show ViewStyle
ViewStyleShow = %runElab derive

%hint
ColorShow : Show Color
ColorShow = %runElab derive

%hint
ViewNodeShow : Show ViewNode
ViewNodeShow = %runElab derive

--

interface Initial t where
  initial : t

interface Initial props => Initial state => Render props state where
  render : props -> state -> (update: (state -> state) -> ViewNode) -> ViewNode

--

record CounterProps where
  constructor MkCounterProps
  step : Int

Initial CounterProps where
  initial = MkCounterProps {step = 1} 

record CounterState where
  constructor MkCounterState
  count : Int

Initial CounterState where
  initial = MkCounterState {count = 0}

Render CounterProps CounterState where
  render props state update = View {onPress = \_ => update {count := state.count + props.step}} [Text (show state.count)]

record AppProps where
  constructor MkAppProps

Initial AppProps where
  initial = MkAppProps

record AppState where
  constructor MkAppState
  counter1 : CounterState
  counter2 : CounterState

Initial AppState where
  initial = MkAppState {counter1 = initial} {counter2 = initial}

Render AppProps AppState where
  render props state update = View [
    render (the CounterProps initial) state.counter1 (\updater => update $ { counter1 $= updater }),
    render (the CounterProps initial) state.counter2 (\updater => update $ { counter2 $= updater })
  ]

main : IO ()
main = do
  putStrLn $ show $ render (the AppProps initial) (the AppState initial) (\_ => Text "TODO")
