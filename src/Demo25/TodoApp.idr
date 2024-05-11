module Demo25.TodoApp

import Demo25.UI.View
import Demo25.UI.Browser.View
import Demo25.UI.Browser.DOM

-- TODO i18n

-- logic

record Todo where
  constructor MakeTodo
  id : Int
  text : String
  isDone : Bool

record Todos where
  constructor MakeTodos
  nextTodoId : Int
  list : List Todo

(.add) : Todos -> { text : String } -> { isDone : Bool } -> Todos
(.add) todos = {
  nextTodoId := todos.nextTodoId + 1,
  list := MakeTodo { id = todos.nextTodoId, text = text, isDone = isDone } :: todos.list
} todos

(.remove) : Todos -> Int -> Todos
(.remove) todos todoId = {
  list := filter (\oldTodo => oldTodo.id /= todoId) todos.list
} todos

(.update) : Todos -> Int -> Todo -> Todos
(.update) todos todoId newTodo = {
  list := (flip map) todos.list (\oldTodo => if oldTodo.id == todoId then newTodo else oldTodo)
} todos

initialTodos = MakeTodos {
  nextTodoId = 3,
  list = [
    MakeTodo { id = 1, text = "Create a new todo", isDone = False },
    MakeTodo { id = 2, text = "Try this app", isDone = True }
  ]
}

data TodosFilter = All | NotDone | Done

Eq TodosFilter where
  All == All = True
  NotDone == NotDone = True
  Done == Done = True
  _ == _ = False

initialTodosFilter : TodosFilter
initialTodosFilter = All

filterTodos : TodosFilter -> Todo -> Bool
filterTodos All todo = True
filterTodos NotDone todo = not todo.isDone
filterTodos Done todo = todo.isDone

-- appearance

data ThemeName = Dark | Light

Eq ThemeName where
  Dark == Dark = True
  Light == Light = True
  _ == _ = False

initialTheme : ThemeName
initialTheme = Dark

record Theme where
  constructor MakeTheme
  backgroundColor : Color
  textColor : Color
  activeTabUnderlineColor : Color
  borderColor : Color
  iconButtonTextColor : Color
  iconButtonBackgroundColor : Color

getTheme : ThemeName -> Theme
getTheme Dark = MakeTheme {
  backgroundColor = rgb 20 20 20,
  textColor = rgb 225 225 225,
  activeTabUnderlineColor = rgb 3 169 244,
  borderColor = rgb 70 70 70,
  iconButtonTextColor = rgb 3 169 244,
  iconButtonBackgroundColor = rgb 10 10 10
}
getTheme Light = MakeTheme {
  backgroundColor = rgb 245 245 245,
  textColor = rgb 60 60 60,
  activeTabUnderlineColor = rgb 33 150 243,
  borderColor = rgb 200 200 200,
  iconButtonTextColor = rgb 33 150 243,
  iconButtonBackgroundColor = rgb 255 255 255
}

themeContext = createContext Theme

IconButton :
  { label : String } ->
  { onPress : List StateUpdate } ->
  View
IconButton = do
  theme <- themeContext
  Flex Row {
    style = s{
      width = dip 24,
      height = dip 24,
      align = Center,
      justify = Center,
      border = s{
        radius = s{ all = 12 },
        width = s{ all = 1},
        color = s{ all = theme.borderColor }
      },
      background = theme.iconButtonBackgroundColor
    },
    press = onPress
  } [
    Flex Row { style = s{ align = Center } } [
      Text {
        style = s{
          color = theme.iconButtonTextColor,
          userSelect = False,
          font = s{ size = 16, weight = 800 }
        }
      } label
    ] 
  ]

Tab :
  { label : String } ->
  { isActive : Bool } ->
  { onPress : List StateUpdate } ->
  View
Tab = do
  theme <- themeContext
  Flex Row {
    style = s{
      justify = Center,
      border = s{
        width = s{ bottom = 4 },
        color = s{
          top = theme.borderColor,
          bottom = if isActive then theme.activeTabUnderlineColor else transparent
        }
      },
      margin = s{ top = 4 },
      padding = s{ vertical = 4, horizontal = 16 }
    },
    press = onPress
  } [
    Text { style = s{ userSelect = False, color = theme.textColor } } label
  ]

-- application

export
TodoApp : View
TodoApp = do
  (themeName, setThemeName) <- useState initialTheme
  let theme = getTheme themeName
  (todos, setTodos) <- useState initialTodos
  (todosFilter, setTodosFilter) <- useState initialTodosFilter
  (newText, setNewText) <- useState $ the String ""
  let filteredTodos = filter (filterTodos todosFilter) todos.list
  let contextProviders = Provider themeContext theme
  contextProviders $ Flex Row {
    style = s{
      justify = Center,
      background = theme.backgroundColor,
      padding = s{ vertical = 16 }
    } 
  } [
    Flex Col {
      style = s {
        border = s{
          width = s{ all = 1 },
          radius = s{ all = 8 },
          color = s{ all = theme.borderColor }
        },
        padding = s{ vertical = 8 }
      }
    } [
       Flex Row {
        style = s{
          width = psf 1.0,
          gap = s{ col = 8 },
          justify = End,
          padding = s{ horizontal = 16 }
        }
      } [
        IconButton {
          label = "☀️",
          onPress = [setThemeName Light]
        },
        IconButton {
          label = "🌙",
          onPress = [setThemeName Dark]
        }
      ],
      Text {
        style = s {
          font = s{ size = 24, weight = 800 },
          align = Center,
          color = theme.textColor
        }
      } "My Todos",
      Flex Row {
        style = s{ 
          width = psf 1.0,
          justify = Center,
          margin = s{ top = 8 }
        }
      } $ (flip map) [
        ("All", All),
        ("Todo", NotDone),
        ("Done", Done)
      ] $ \(label, key) => Tab {
        label = label,
        isActive = todosFilter == key,
        onPress = [setTodosFilter key]
      },
      Flex Row {
        style = s{
          gap = s{ col = 16 },
          padding = s { vertical = 4, horizontal = 16 },
          border = s{
            width = s{ vertical = 1 },
            color = s{ all = theme.borderColor }
          },
          width = psf 1.0
        }
      } [
        Flex Row { style = s{ width = dip 24 } } [],
        Flex Row { style = s{ padding = s{ vertical = 4 }, grow = 1 } } [
          Input { 
            value = newText, 
            change = \value => [setNewText value],
            style = s{ color = theme.textColor }
          }
        ],
        IconButton {
          label = "+",
          onPress = [
            setTodos $ todos.add {
              text = newText,
              isDone = case todosFilter of All => False; Done => True; NotDone => False
            },
            setNewText ""
          ]
        }
      ],
      Flex Col { style = s{ width = psf 1.0 } } $ (flip map) filteredTodos $ \todo =>
        Flex Row {
          style = s{
            gap = s{ col = 16 },
            padding = s { vertical = 4, horizontal = 16 },
            border = s{
              width = s{ bottom = 1 },
              color = s{ all = theme.borderColor }
            },
            width = psf 1.0
          }
        } [
          IconButton {
            label = if todo.isDone then "✔" else " ",
            onPress = [setTodos $ todos.update todo.id $ { isDone $= not } todo]
          },
          Flex Row { style = s{ grow = 1, padding = s{ vertical = 4 } } } [
            Text { style = s{ color = theme.textColor } } todo.text
          ],
          IconButton {
            label = "✗",
            onPress = [setTodos $ todos.remove todo.id]
          }
        ]
    ]
  ]

--- render

main : IO ()
main = do
  bodyStyle <- (!((!((!window).document)).body)).style
  bodyStyle.set "margin" "0"
  bodyStyle.set "fontFamily" "Roboto, Arial"
  root <- Root.create
  root.render [
    TodoApp
  ]
