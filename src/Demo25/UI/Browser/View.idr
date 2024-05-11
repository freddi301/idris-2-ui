module Demo25.UI.Browser.View

import Data.List.Extra
import Data.SortedMap.Dependent
import Data.SortedMap
import Data.Ref

import Demo25.UI.View
import Demo25.UI.Style
import Demo25.UI.Browser.DOM
import Demo25.UI.Browser.Style

-- TODO do not attach event listneres directly to html elements, instead to root element
-- TODO clean states after a render (remove unmounted)

viewToElementTag : View -> String
viewToElementTag (Text _) = "span"
viewToElementTag (Input _ _) = "input"
viewToElementTag (Flex _ _) = "div"
viewToElementTag _ = ?h2

indexMaybe : Nat -> List a -> Maybe a
indexMaybe Z (head :: tail) = Just head
indexMaybe (S i) (head :: tail) = indexMaybe i tail
indexMaybe _ [] = Nothing

parameters (update : List StateUpdate -> IO ())
  create : (view : View) -> IO (Element (viewToElementTag view))
  create (Text { style, press } content) = do
    element <- (!(!window).document).createElement "span"
    element.innerText_set content
    element.onclick_set $ \event => update press
    updateTextStyle element defaultTextStyle style
    pure element
  create (Flex direction { style } children) = do
    element <- (!(!window).document).createElement "div"
    sequence_ $ map (\view => do element.appendChild !(create view)) children
    updateFlexStyle element defaultFlexStyle style
    style <- element.style
    style.set "display" "flex"
    style.set "flexDirection" (case direction of Col => "column"; Row => "row")
    pure element
  create (Input value change) = do
    element <- (!(!window).document).createElement "input"
    element.value_set value
    element.oninput_set $ \event => update (change !(!event.currentTarget).value)
    pure element
  create _ = ?h3

  patch :
    (HasChildren tag) => (parentElement : Element tag) ->
    (oldView : View) -> (oldElement : Element (viewToElementTag oldView)) ->
    (newView : View) -> IO ()
  patch parentElement (Text { style = oldStyle } oldContent) oldElement (Text { style = newStyle, press } newContent) = do
    if newContent /= oldContent then oldElement.innerText_set newContent else pure ()
    oldElement.onclick_set $ \event => update press
    updateTextStyle oldElement oldStyle newStyle
  patch parentElement (Input oldValue _) oldElement (Input newValue newChange) = do
    if newValue /= oldValue then oldElement.value_set newValue else pure ()
    oldElement.oninput_set $ \event => update (newChange !(!event.currentTarget).value)
  patch parentElement (Flex oldDirection { style = oldStyle } oldChildren) oldElement (Flex newDirection { style = newStyle } newChildren) = do
    if newDirection /= oldDirection then (!oldElement.style).set "flexDirection" (case newDirection of Col => "column"; Row => "row") else pure ()
    childrenElements <- oldElement.children
    let
      perChild : Nat -> IO ()
      perChild index with (indexMaybe index childrenElements, indexMaybe index oldChildren, indexMaybe index newChildren)
        _ | (Just (_ ** oldChildElement), Just oldChild, Just newChild) = patch oldElement oldChild (believe_me oldChildElement) newChild
        _ | (Nothing, Nothing, Just newChild) = oldElement.appendChild !(create newChild)
        _ | (Just (_ ** oldChildElement), Just oldChild, Nothing) = oldElement.removeChild oldChildElement
        _ | (Nothing, Nothing, Nothing) = pure ()
        _ | (x, y, z) = throw "Should not get here while patching dom"
    sequence_ $ map perChild [0..(max (length oldChildren) (length newChildren))]
    updateFlexStyle oldElement oldStyle newStyle
  patch parentElement oldView oldElement newView =
    parentElement.replaceChild !(create newView) oldElement

namespace Root

  data Root : Type where
    MakeRoot :
      (rootElement : Element "div") ->
      (currentStates : IORef $ SortedMap (List (String, String)) (identity : String ** Cell identity)) ->
      (currentViews : IORef $ List View) ->
      Root

  update : Root -> List View -> (updates : List StateUpdate) -> IO ()
  update root@(MakeRoot rootElement currentStates currentViews) views updates = do
    let oldStates = !(readRef currentStates)
    let newStates = foldl (\states => \(MakeStateUpdate path identity value) => insert path (identity ** MakeCell value) states) oldStates updates
    writeRef currentStates newStates
    let oldViews = !(readRef currentViews)
    let newViews = (flip mapi) views $ \index => \view => unfold empty newStates [("root", show index)] view
    writeRef currentViews newViews
    patch (update root views) rootElement (Flex Col oldViews) rootElement (Flex Col newViews)

  export
  create : IO Root
  create = do
    document <- (!window).document
    element <- document.createElement "div"
    (!document.body).appendChild element
    let root = (MakeRoot element !(newRef empty) !(newRef []))
    pure root

  export
  (.render) : Root -> List View -> IO ()
  (.render) root views = update root views []
  