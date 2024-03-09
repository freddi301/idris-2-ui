module Demo1
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

record ViewState where
  constructor MkViewState
  inputText : String
  todos : List String

view1 : ViewState -> ViewNode
view1 state = View [
    Text "Todo app demo",
    View {style = Style {flexDirection = Row}} [
      TextInput {value = state.inputText} {onChange = \text => view1 $ {inputText := text} state},
      View {onPress = \_ => view1 $ {todos := state.inputText :: state.todos} state}{style = Style {borderWidth = 1}} [Text "Add"],
      ViewList [View [Text todo] | todo <- state.todos]
    ]
  ]


main : IO ()
main = do
  putStrLn $ show $ view1 $ (MkViewState {inputText = ""} {todos = []})


