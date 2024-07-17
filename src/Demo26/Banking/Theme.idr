module Demo26.Banking.Theme

import Demo26.UI.View

public export
record Theme where
  constructor MakeTheme
  primaryTextColor : Color
  backgroundColor : Color
  linkTextColor : Color
  tabTextColor : Color
  tabActiveTextColor : Color

Light : Theme
Light = MakeTheme {
  primaryTextColor = rgb 0 0 0,
  backgroundColor = rgb 255 255 255,
  linkTextColor = rgb 0 0 255,
  tabTextColor = rgb 125 125 125,
  tabActiveTextColor = rgb 0 0 255
}

Dark : Theme
Dark = MakeTheme {
  primaryTextColor = rgb 255 255 255,
  backgroundColor = rgb 0 0 0,
  linkTextColor = rgb 0 0 255,
  tabTextColor = rgb 125 125 125,
  tabActiveTextColor = rgb 0 0 255
}

namespace ThemeName

  public export
  data ThemeName = Light | Dark

export
Eq ThemeName where
  (==) Light Light = True
  (==) Dark Dark = True
  (==) _ _ = False

theme : ThemeName -> Theme
theme Light = Light
theme Dark = Dark

ThemeNameContext = createContext ThemeName
SetThemeNameContext = createContext (ThemeName -> StateUpdate)

export
useThemeName : Exposed (ThemeName, ThemeName -> StateUpdate)
useThemeName = Expose $ \expose => do
  name <- ThemeNameContext
  setName <- SetThemeNameContext
  expose (name, setName)

export
provideTheme : View -> View
provideTheme child = do
  (themeName, setThemeName) <- useState $ the ThemeName Light
  Provider ThemeNameContext themeName $ Provider SetThemeNameContext setThemeName $
  child

export
useTheme : Exposed Theme
useTheme = Expose $ \expose => do
  name <- ThemeNameContext
  expose $ Theme.theme name
