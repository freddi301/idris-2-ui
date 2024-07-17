module Demo26.Banking.Main

import Data.So

import Demo26.UI.View
import Demo26.UI.Browser.View
import Demo26.UI.Browser.DOM

import Demo26.Banking.Domain
import Demo26.Banking.Theme
import Demo26.Banking.Component
import Demo26.Banking.Route
import Demo26.Banking.Screen
import Demo26.Banking.Internationalization

export
App : View
App =
  provideLocalization $
  provideTheme $
  provideRoute $ \route =>
    MobileFrame [
      Flex { style = s { width = psf 1.0, height = psf 1.0 } } [
        Flex { style = s { width = psf 1.0, grow = 1 } } [ Screen route ],
        Navigation
      ]
    ]

covering
main : IO ()
main = do
  bodyStyle <- (!((!((!window).document)).body)).style
  bodyStyle.set "margin" "0"
  root <- Root.create
  root.render [
    App
  ]