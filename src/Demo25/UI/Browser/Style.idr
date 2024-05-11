module Demo25.UI.Browser.Style

import Data.Fin

import Demo25.UI.Style
import Demo25.UI.Browser.DOM

updateStyleFactory : style_t -> style_t -> { attribute : Type } -> Eq attribute => (style_t -> attribute) -> (attribute -> IO ()) -> IO ()
updateStyleFactory oldStyle newStyle getAttribute updateAttribute = do
  let oldAttribute = getAttribute oldStyle
  let newAttribute = getAttribute newStyle
  if (newAttribute /= oldAttribute) then updateAttribute newAttribute else pure ()  

doubleDipToRem : Double -> String
doubleDipToRem value = "\{show $ value / 16}rem"

dipToRem : DensitiyIndipendentPixels -> String
dipToRem value = doubleDipToRem $ cast value

colorToRGBA : Color -> String
colorToRGBA value =
  let (r, g, b, a) = toRGBA value
  in "rgba(\{show $ finToInteger r}, \{show $ finToInteger g}, \{show $ finToInteger b}, \{show a})"

boxSizeUnitToCSSUnit : BoxSizeUnit -> String
boxSizeUnitToCSSUnit (BoxSizeUnitDensitiyIndipendentPixels dip) = doubleDipToRem dip
boxSizeUnitToCSSUnit (BoxSizeUnitParentSizeFraction psf) = "\{show $ psf * 100}%"

export
updateTextStyle : Element tag -> TextStyle -> TextStyle -> IO ()
updateTextStyle element oldStyle newStyle = do
  style <- element.style
  let updateStyle = updateStyleFactory oldStyle newStyle
  updateStyle (\style => style.font.family) $ \value => do
    style.set "fontFamily" value
  updateStyle (\style => style.font.size) $ \value => do
    style.set "fontSize" $ dipToRem value
  updateStyle (\style => style.font.weight) $ \value => do
    style.set "fontWeight" (show (fst value))
  updateStyle (\style => style.font.style) $ \value => do
    let fontStyle = case value of Normal => "normal"; Italic => "italic"
    style.set "fontStyle" fontStyle
  updateStyle (\style => style.color) $ \value => do
    style.set "color" $ colorToRGBA value
  updateStyle (\style => style.align) $ \value => do
    let textAlign = case value of Left => "left"; Right => "right"; Center => "center"; Justify => "justify"
    style.set "textAlign" textAlign
  updateStyle (\style => style.lineHeight) $ \value => do
    style.set "lineHeight" (show value)

export
updateFlexStyle : Element tag -> FlexStyle -> FlexStyle -> IO ()
updateFlexStyle element oldStyle newStyle = do
  style <- element.style
  let updateStyle = updateStyleFactory oldStyle newStyle
  updateStyle (\style => style.margin.top) $ \value => do
    style.set "marginTop" $ dipToRem value
  updateStyle (\style => style.margin.right) $ \value => do
    style.set "marginRight" $ dipToRem value
  updateStyle (\style => style.margin.bottom) $ \value => do
    style.set "marginBottom" $ dipToRem value
  updateStyle (\style => style.margin.left) $ \value => do
    style.set "marginLeft" $ dipToRem value
  updateStyle (\style => style.padding.top) $ \value => do
    style.set "paddingTop" $ dipToRem value
  updateStyle (\style => style.padding.right) $ \value => do
    style.set "paddingRight" $ dipToRem value
  updateStyle (\style => style.padding.bottom) $ \value => do
    style.set "paddingBottom" $ dipToRem value
  updateStyle (\style => style.padding.left) $ \value => do
    style.set "paddingLeft" $ dipToRem value
  updateStyle (\style => style.border.width.top) $ \value => do
    style.set "borderTopWidth" $ dipToRem value
  updateStyle (\style => style.border.width.right) $ \value => do
    style.set "borderRightWidth" $ dipToRem value
  updateStyle (\style => style.border.width.bottom) $ \value => do
    style.set "borderBottomWidth" $ dipToRem value
  updateStyle (\style => style.border.width.left) $ \value => do
    style.set "borderLeftWidth" $ dipToRem value
  updateStyle (\style => style.border.color.top) $ \value => do
    style.set "borderTopColor" $ colorToRGBA value
  updateStyle (\style => style.border.color.right) $ \value => do
    style.set "borderRightColor" $ colorToRGBA value
  updateStyle (\style => style.border.color.bottom) $ \value => do
    style.set "borderBottomColor" $ colorToRGBA value
  updateStyle (\style => style.border.color.left) $ \value => do
    style.set "borderLeftColor" $ colorToRGBA value
  updateStyle (\style => style.border.radius.topLeft) $ \value => do
    style.set "borderTopLeftRadius" $ dipToRem value
  updateStyle (\style => style.border.radius.topRight) $ \value => do
    style.set "borderTopRightRadius" $ dipToRem value
  updateStyle (\style => style.border.radius.bottomRight) $ \value => do
    style.set "borderBottomRightRadius" $ dipToRem value
  updateStyle (\style => style.border.radius.bottomLeft) $ \value => do
    style.set "borderBottomLeftRadius" $ dipToRem value
  updateStyle (\style => style.background) $ \value => do
    style.set "backgroundColor" $ colorToRGBA value
  updateStyle (\style => style.gap.row) $ \value => do
    style.set "rowGap" $ dipToRem value
  updateStyle (\style => style.gap.col) $ \value => do
    style.set "columnGap" $ dipToRem value
  updateStyle (\style => style.wrap) $ \value => do
    case value of
      True => style.set "flexWrap" "wrap"
      False => style.set "flexWrap" "nowrap"
  updateStyle (\style => style.justify) $ \value => do
    let justifyContent = case value of Start => "flex-start"; End => "flex-end"; Center => "center" 
    style.set "justifyContent" justifyContent
  updateStyle (\style => style.align) $ \value => do
    let alignSelf = case value of Start => "flex-start"; End => "flex-end"; Center => "center" 
    style.set "alignSelf" alignSelf
  updateStyle (\style => style.grow) $ \value => do
    style.set "flexGrow" $ show value
  updateStyle (\style => style.width) $ \value => do
    if value.minimum == value.maximum
      then do
        style.set "width" $ boxSizeUnitToCSSUnit value.minimum
      else do
        style.set "minWidth" $ boxSizeUnitToCSSUnit value.minimum
        style.set "maxWidth" $ boxSizeUnitToCSSUnit value.maximum
  updateStyle (\style => style.height) $ \value => do
    if value.minimum == value.maximum
      then do
        style.set "height" $ boxSizeUnitToCSSUnit value.minimum
      else do
        style.set "minHeight" $ boxSizeUnitToCSSUnit value.minimum
        style.set "maxHeight" $ boxSizeUnitToCSSUnit value.maximum




