module Demo26.Banking.Component

import Demo26.UI.View

import Demo26.Banking.Route
import Demo26.Banking.Theme

export
MobileFrame : List View -> View
MobileFrame contents = do
  theme <- useTheme
  Flex {
    style = s {
      width = dip 332,
      height = dip 632,
      background = rgb 0 0 0,
      padding = s { all = 16 },
      border = s { radius = s { all = 16 } }
    }
  } [
    Flex {
      style = s {
        width = psf 1.0,
        height = psf 1.0,
        background = theme.backgroundColor
      }
    } contents
  ]

export
Link : (to : Route) -> (label : String) -> View
Link to label = do
  setRoute <- useNavigate
  theme <- useTheme
  Text {
    press = [setRoute to],
    style = s { color = theme.linkTextColor }
  } label

export
Tabs : List (Route, String) -> View
Tabs tabs = do
  route <- useRoute
  navigate <- useNavigate
  theme <- useTheme
  Flex {
    style = s {
      direction = Row,
      width = psf 1.0,
      border = s {
        width = s { top = 1 },
        color = s { top = theme.tabTextColor }
      }
    }
  } $ tabs <&> \(tabRoute, label) => do
    Flex {
      style = s {
        width = psf $ 1.0 / (cast $ length tabs),
        padding = s { all = 8 },
        direction = Row,
        justify = Center
      }
    } [
      Text {
        press = [navigate tabRoute],
        style = s {
          color = if tabRoute == route then theme.tabActiveTextColor else theme.tabTextColor
        }
      } label
    ]


export
Button : {onPress: List StateUpdate} -> (label : String) -> View
Button {onPress} label = do
  theme <- useTheme
  Flex {
    style = s {
      background = theme.backgroundColor,
      padding = s { all = 8 },
      border = s {
        radius = s { all = 4 },
        width = s { all = 1 },
        color = s { all = theme.primaryTextColor }
      }
    }
  } [
    Text {
      press = onPress,
      style = s {
        color = theme.primaryTextColor
      }
    } label
  ]

public export
record Column (item : Type) where
  constructor MakeColumn
  header : String
  cell : item -> String

export
Table : List item -> List (Column item) -> View
Table rows columns = do
  theme <- useTheme
  let columnWidth : BoxSize = psf $ 1.0 / (cast $ length columns)
  Flex { style = s { width = psf 1.0 } } [
    Flex {
      style = s {
        direction = Row,
        width = psf 1.0
      }
    } $ columns <&> \column => Flex {
        style = s {
          width = columnWidth,
          padding = s { all = 8 }
        }
      } [
        Text {
          style = s {
            color = theme.primaryTextColor
          }
        } column.header
      ],
    Flex { style = s { width = psf 1.0 } } $ rows <&> \row => Flex {
        style = s {
          width = psf 1.0,
          direction = Row
        }
      } $ columns <&> \column => Flex {
          style = s {
            width = columnWidth,
            padding = s { all = 8 },
            justify = End
          }
        } [
          Text {
            style = s {
              color = theme.primaryTextColor
            }
          } (column.cell row)
        ]
  ]

export
Section : String -> View
Section title = do
  theme <- useTheme
  Flex {
    style = s {
      padding = s { horizontal = 16, vertical = 8 }
    }
  } [
    Text {
      style = s {
        color = theme.primaryTextColor,
        font = s {
          size = 18
        }
      }
    } title
  ]

export
Radio : Eq v => {value : v} -> {onChange : v -> StateUpdate} -> {options : List (v, String)} -> View
Radio = do
  theme <- useTheme
  Flex $ options <&> \(val, label) =>
    Flex { press = [onChange val], style = s { direction = Row, padding = s { horizontal = 16 } , gap = s { col = 8 } } } [
      Flex {
        style = s {
          width = dip 8,
          height = dip 8,
          align = Center,
          background = if val == value then theme.primaryTextColor else theme.backgroundColor,
          border = s {
            radius = s { all = 4 },
            width = s { all = 1 },
            color = s { all = theme.primaryTextColor }
          }
        }
      } [],
      Flex [
        Text {
          style = s {
            color = theme.primaryTextColor
          }
        } label
      ]
    ]