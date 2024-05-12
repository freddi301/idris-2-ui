module Demo25.UI.Style

import Data.Fin
import Data.Double
import Language.Reflection

-- inspired by https://reactnative.dev/docs/text-style-props https://reactnative.dev/docs/view-style-props

namespace DensitiyIndipendentPixels

  export
  data DensitiyIndipendentPixels = MakeDensitiyIndipendentPixels Double

  export
  fromInteger : (x : Integer) -> DensitiyIndipendentPixels
  fromInteger x = MakeDensitiyIndipendentPixels (cast x) 

  export
  fromDouble : (x : Double) -> DensitiyIndipendentPixels
  fromDouble x = MakeDensitiyIndipendentPixels x

  export
  Cast DensitiyIndipendentPixels Double where
    cast (MakeDensitiyIndipendentPixels x) = x

  export
  Cast Double DensitiyIndipendentPixels where
    cast x = (MakeDensitiyIndipendentPixels x)
  
  export
  Eq DensitiyIndipendentPixels where
    (==) (MakeDensitiyIndipendentPixels x) (MakeDensitiyIndipendentPixels y) = x == y

namespace Color

  export
  data Color : Type where
    RGBA : Fin 256 -> Fin 256 -> Fin 256 -> Double -> Color

  export
  Eq Color where
    (==) (RGBA xr xg xb xa) (RGBA yr yg yb ya) = xr == yr && xg == yg && xb == yb && xa == ya


  export
  rgb : (red : Fin 256) -> (green : Fin 256) -> (blue : Fin 256) -> Color
  rgb red green blue = RGBA red green blue 1

  export
  rgba : (red : Fin 256) -> (green : Fin 256) -> (blue : Fin 256) -> (alpha : Double) -> Color
  rgba = RGBA

  export
  hex : (colorHexString : String) -> { auto l : strLength colorHexString = 6 } -> Color
  hex string = RGBA 0 0 0 1 -- TODO

  export
  transparent : Color
  transparent = rgba 0 0 0 0

  export
  toRGBA : Color -> (Fin 256, Fin 256, Fin 256, Double)
  toRGBA (RGBA r g b a) = (r, g, b, a)

namespace FontStyle

  public export
  data FontStyle = Normal | Italic

  export
  Eq FontStyle where
    (==) Normal Normal = True
    (==) Italic Italic = True
    (==) _ _ = False

namespace FotnWeight

  export
  interface FontWeightInterface (weight : Int) where

  export FontWeightInterface 100 where
  export FontWeightInterface 200 where
  export FontWeightInterface 300 where
  export FontWeightInterface 400 where
  export FontWeightInterface 500 where
  export FontWeightInterface 600 where
  export FontWeightInterface 700 where
  export FontWeightInterface 800 where

  export
  fromInteger : (x : Integer) -> {auto i : FontWeightInterface (cast x)} -> (fw : Int ** FontWeightInterface fw)
  fromInteger x {i} = ((cast x) ** i)

  export
  Eq (fw : Int ** FontWeightInterface fw) where
    (==) x y = (fst x) == (fst y) 


namespace Font

  public export
  record Font where
    constructor MakeFont
    family : String
    size : DensitiyIndipendentPixels
    weight : (fw : Int ** FontWeightInterface fw)
    style : FontStyle

  defaultFont : Font
  defaultFont = MakeFont {
    family = "",
    size = 16,
    weight = 400,
    style = Normal
  }

  export
  s :
    { default defaultFont.family family : String } ->
    { default defaultFont.size size : DensitiyIndipendentPixels } ->
    { default defaultFont.weight weight : (fw : Int ** FontWeightInterface fw) } ->
    { default defaultFont.style style : FontStyle } ->
    Font
  s { family, size, weight, style } = MakeFont { family = family, size = size, weight = weight, style = style }

namespace TextAlign

  public export
  data TextAlign : Type where
    Left : TextAlign
    Right : TextAlign
    Center : TextAlign
    Justify : TextAlign

  export
  Eq TextAlign where
    (==) Left Left = True
    (==) Right Right = True
    (==) Center Center = True
    (==) Justify Justify = True
    (==) _ _ = False


namespace TextStyle

  public export
  record TextStyle where
    constructor MakeTextStyle
    font : Font
    color : Color
    align : TextAlign
    lineHeight : Double
    userSelect : Bool

  export
  defaultTextStyle : TextStyle
  defaultTextStyle = MakeTextStyle {
    font = defaultFont,
    color = rgb 0 0 0,
    align = Left,
    lineHeight = 1.2,
    userSelect = True
  }

  export
  s :
    { default defaultTextStyle.font font : Font } ->
    { default defaultTextStyle.color color : Color } ->
    { default defaultTextStyle.align align : TextAlign } ->
    { default defaultTextStyle.lineHeight lineHeight : Double } ->
    { default defaultTextStyle.userSelect userSelect : Bool } ->
    TextStyle
  s { font, color, align, lineHeight } = MakeTextStyle { font = font, color = color, align = align, lineHeight = lineHeight, userSelect = userSelect }

namespace InputStyle

  public export
  record InputStyle where
    constructor MakeInputStyle
    color : Color

  export
  defaultInputStyle : InputStyle
  defaultInputStyle = MakeInputStyle {
    color = rgb 0 0 0
  }

  export
  s :
    { default defaultInputStyle.color color : Color } ->
    InputStyle
  s { color } = MakeInputStyle { color = color }

namespace Margin

  public export
  record Margin where
    constructor MakeMargin
    top : DensitiyIndipendentPixels
    right : DensitiyIndipendentPixels
    bottom : DensitiyIndipendentPixels
    left : DensitiyIndipendentPixels

  defaultMargin : Margin
  defaultMargin = MakeMargin { top = 0, right = 0, bottom = 0, left = 0 }

  export
  s : 
    { default 0 all : DensitiyIndipendentPixels } -> 
    { default all horizontal : DensitiyIndipendentPixels } -> 
    { default all vertical : DensitiyIndipendentPixels } -> 
    { default vertical top : DensitiyIndipendentPixels } ->
    { default horizontal right : DensitiyIndipendentPixels } ->
    { default vertical bottom : DensitiyIndipendentPixels } ->
    { default horizontal left : DensitiyIndipendentPixels } ->
    Margin
  s { top, right, bottom, left } = MakeMargin { top = top, right = right, bottom = bottom, left = left } 

namespace Padding

  public export
  record Padding where
    constructor MakePadding
    top : DensitiyIndipendentPixels
    right : DensitiyIndipendentPixels
    bottom : DensitiyIndipendentPixels
    left : DensitiyIndipendentPixels

  defaultPadding : Padding
  defaultPadding = MakePadding { top = 0, right = 0, bottom = 0, left = 0 }

  export
  s :
    { default 0 all : DensitiyIndipendentPixels } -> 
    { default all horizontal : DensitiyIndipendentPixels } -> 
    { default all vertical : DensitiyIndipendentPixels } -> 
    { default vertical top : DensitiyIndipendentPixels } ->
    { default horizontal right : DensitiyIndipendentPixels } ->
    { default vertical bottom : DensitiyIndipendentPixels } ->
    { default horizontal left : DensitiyIndipendentPixels } ->
    Padding
  s { top, right, bottom, left } = MakePadding { top = top, right = right, bottom = bottom, left = left }


namespace BorderWidth

  public export
  record BorderWidth where
    constructor MakeBorderWidth
    top : DensitiyIndipendentPixels
    right : DensitiyIndipendentPixels
    bottom : DensitiyIndipendentPixels
    left : DensitiyIndipendentPixels

  defaultBorderWidth : BorderWidth
  defaultBorderWidth = MakeBorderWidth { top = 0, right = 0, bottom = 0, left = 0 }

  export
  s :
    { default 0 all : DensitiyIndipendentPixels } -> 
    { default all horizontal : DensitiyIndipendentPixels } -> 
    { default all vertical : DensitiyIndipendentPixels } -> 
    { default vertical top : DensitiyIndipendentPixels } ->
    { default horizontal right : DensitiyIndipendentPixels } ->
    { default vertical bottom : DensitiyIndipendentPixels } ->
    { default horizontal left : DensitiyIndipendentPixels } ->
    BorderWidth
  s { top, right, bottom, left } = MakeBorderWidth { top = top, right = right, bottom = bottom, left = left }

namespace BorderColor

  public export
  record BorderColor where
    constructor MakeBorderColor
    top : Color
    right : Color
    bottom : Color
    left : Color

  defaultBorderColor : BorderColor
  defaultBorderColor = MakeBorderColor {
    top = rgb 255 255 255,
    right = rgb 255 255 255,
    bottom = rgb 255 255 255,
    left = rgb 255 255 255
  }

  export
  s :
    { default (rgb 255 255 255) all : Color } -> 
    { default all horizontal : Color } -> 
    { default all vertical : Color } -> 
    { default vertical top : Color } ->
    { default horizontal right : Color } ->
    { default vertical bottom : Color } ->
    { default horizontal left : Color } ->
    BorderColor
  s { top, right, bottom, left } = MakeBorderColor { top = top, right = right, bottom = bottom, left = left }


namespace BorderRadius
  
  public export
  record BorderRadius where
    constructor MakeBorderRadius
    topLeft : DensitiyIndipendentPixels
    topRight : DensitiyIndipendentPixels
    bottomRight : DensitiyIndipendentPixels
    bottomLeft : DensitiyIndipendentPixels

  defaultBorderRadius : BorderRadius
  defaultBorderRadius = MakeBorderRadius { topLeft = 0, topRight = 0, bottomRight = 0, bottomLeft = 0 }
 
  max : DensitiyIndipendentPixels -> DensitiyIndipendentPixels -> DensitiyIndipendentPixels
  max x y = cast (max (the Double $ cast x) (the Double $ cast y))
  
  export
  s :
    { default 0 all : DensitiyIndipendentPixels } -> 
    { default all top : DensitiyIndipendentPixels } -> 
    { default all left : DensitiyIndipendentPixels } -> 
    { default all bottom : DensitiyIndipendentPixels } -> 
    { default all right : DensitiyIndipendentPixels } -> 
    { default (max top left) topLeft : DensitiyIndipendentPixels } ->
    { default (max top right) topRight : DensitiyIndipendentPixels } ->
    { default (max bottom right) bottomRight : DensitiyIndipendentPixels } ->
    { default (max bottom left) bottomLeft : DensitiyIndipendentPixels } ->
    BorderRadius
  s { topLeft, topRight, bottomRight, bottomLeft } = MakeBorderRadius { topLeft = topLeft, topRight = topRight, bottomRight = bottomRight, bottomLeft = bottomLeft }

namespace Border

  public export
  record Border where
    constructor MakeBorder
    width : BorderWidth
    color : BorderColor
    radius : BorderRadius

  defaultBorder : Border
  defaultBorder = MakeBorder {
    width = defaultBorderWidth,
    color = defaultBorderColor,
    radius = defaultBorderRadius
  }

  export
  s : 
    { default defaultBorder.width width : BorderWidth } ->
    { default defaultBorder.color color : BorderColor } ->
    { default defaultBorder.radius radius : BorderRadius } ->
    Border
  s { width, color, radius } = MakeBorder { width = width, color = color, radius = radius } 

namespace FlexGap

  public export
  record FlexGap where
    constructor MakeFlexGap
    col : DensitiyIndipendentPixels
    row : DensitiyIndipendentPixels

  defaultFlexGap : FlexGap
  defaultFlexGap = MakeFlexGap { col = 0, row = 0 }

  export
  s :
    { default 0 all : DensitiyIndipendentPixels } -> 
    { default all col : DensitiyIndipendentPixels } -> 
    { default all row : DensitiyIndipendentPixels } -> 
    FlexGap
  s { col, row } = MakeFlexGap { col = col, row = row }

namespace FlexJustify

  public export
  data FlexJustify = Start | End | Center

  export
  Eq FlexJustify where
    (==) Start Start = True
    (==) End End = True
    (==) Center Center = True
    (==) _ _ = False


namespace FlexAlign

  public export
  data FlexAlign = Start | End | Center

  export
  Eq FlexAlign where
    (==) Start Start = True
    (==) End End = True
    (==) Center Center = True
    (==) _ _ = False

namespace BoxSize

  public export
  data BoxSizeUnit = BoxSizeUnitDensitiyIndipendentPixels Double | BoxSizeUnitParentSizeFraction Double

  export
  Eq BoxSizeUnit where
    (==) (BoxSizeUnitDensitiyIndipendentPixels x) (BoxSizeUnitDensitiyIndipendentPixels y) = x == y
    (==) (BoxSizeUnitParentSizeFraction x) (BoxSizeUnitParentSizeFraction y) = x == y
    (==) _ _ = False

  export
  dip : Double -> BoxSizeUnit
  dip value = BoxSizeUnitDensitiyIndipendentPixels value

  export
  psf : Double -> BoxSizeUnit
  psf value = BoxSizeUnitParentSizeFraction value

  public export
  record BoxSize where
    constructor MakeBoxSize
    minimum : BoxSizeUnit
    maximum : BoxSizeUnit

  export
  MinBoxSizeUnitDensitiyIndipendentPixels : Double
  MinBoxSizeUnitDensitiyIndipendentPixels = 0

  export
  MaxBoxSizeUnitDensitiyIndipendentPixels : Double
  MaxBoxSizeUnitDensitiyIndipendentPixels = 1 / 0

  defaultBoxSize : BoxSize
  defaultBoxSize = MakeBoxSize {
    minimum = BoxSizeUnitDensitiyIndipendentPixels MinBoxSizeUnitDensitiyIndipendentPixels,
    maximum = BoxSizeUnitDensitiyIndipendentPixels MaxBoxSizeUnitDensitiyIndipendentPixels
  }

  export
  Eq BoxSize where
    (==) x y = x.minimum == y.minimum && x.maximum == y.maximum
 
  namespace Exact

    export
    dip : Double -> BoxSize
    dip size = MakeBoxSize { minimum = dip size, maximum = dip size }

    export
    psf : Double -> BoxSize
    psf size = MakeBoxSize { minimum = psf size, maximum = psf size }

  export
  s :
    { default defaultBoxSize.minimum min : BoxSizeUnit } ->
    { default defaultBoxSize.maximum max : BoxSizeUnit } ->
    BoxSize
  s { min, max } = MakeBoxSize { minimum = min, maximum = max }

namespace FlexDirection

  public export
  data FlexDirection = Row | Col

  export
  Eq FlexDirection where
    Row == Row = True
    Col == Col = True
    _ == _ = False

namespace FlexStyle

  public export
  record FlexStyle where
    constructor MakeFlexStyle
    direction : FlexDirection
    margin : Margin
    padding : Padding
    border : Border
    background : Color
    gap : FlexGap
    wrap : Bool
    justify : FlexJustify
    align : FlexAlign
    grow : Double
    width : BoxSize
    height : BoxSize

  export
  defaultFlexStyle : FlexStyle
  defaultFlexStyle = MakeFlexStyle {
    direction = Col,
    margin = defaultMargin,
    padding = defaultPadding,
    border = defaultBorder,
    background = rgba 255 255 255 0,
    gap = defaultFlexGap,
    wrap = False,
    justify = Start,
    align = Start,
    grow = 0,
    width = defaultBoxSize,
    height = defaultBoxSize
  }

  export
  s : 
    { default defaultFlexStyle.direction direction : FlexDirection } ->
    { default defaultFlexStyle.margin margin : Margin } ->
    { default defaultFlexStyle.padding padding : Padding } ->
    { default defaultFlexStyle.border border : Border } ->
    { default defaultFlexStyle.background background : Color } ->
    { default defaultFlexStyle.gap gap : FlexGap } ->
    { default defaultFlexStyle.wrap wrap : Bool } ->
    { default defaultFlexStyle.justify justify : FlexJustify } ->
    { default defaultFlexStyle.align align : FlexAlign } ->
    { default defaultFlexStyle.grow grow : Double } ->
    { default defaultFlexStyle.width width : BoxSize } ->
    { default defaultFlexStyle.height height : BoxSize } ->
    FlexStyle
  s { margin, padding, border, background, gap, wrap, justify, align, grow, width, height } =
    MakeFlexStyle { direction = direction, margin = margin, padding = padding, border = border, background = background, gap = gap, wrap = wrap, justify = justify, align = align, grow = grow, width = width, height = height}
