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
-- TODO reorder list items by key

viewToElementTag : View -> String
viewToElementTag (Text _) = "span"
viewToElementTag (Input _ _) = "input"
viewToElementTag (Flex _) = "div"
viewToElementTag _ = ?h2

indexMaybe : Nat -> List a -> Maybe a
indexMaybe Z (head :: tail) = Just head
indexMaybe (S i) (head :: tail) = indexMaybe i tail
indexMaybe _ [] = Nothing

orDefault : Lazy a -> Maybe a -> a
orDefault d Nothing = d
orDefault d (Just x) = x

parameters (update : List StateUpdate -> IO ())
  create : (view : View) -> IO (Element (viewToElementTag view))
  create (Text { style, press } content) = do
    element <- (!(!window).document).createElement "span"
    element.innerText_set content
    element.onclick_set $ \event => update press
    updateTextStyle element Nothing style
    pure element
  create (Input { style } value change) = do
    element <- (!(!window).document).createElement "input"
    element.value_set value
    element.oninput_set $ \event => update (change !(!event.currentTarget).value)
    updateInputStyle element Nothing style
    pure element
  create (Flex { style, press } children) = do
    element <- (!(!window).document).createElement "div"
    sequence_ $ map (\view => do element.appendChild !(create view)) children
    element.onclick_set $ \event => update press
    updateFlexStyle element Nothing style
    pure element
  create _ = ?h3

  patch :
    (HasChildren tag) => (parentElement : Element tag) ->
    (oldView : View) -> (oldElement : Element (viewToElementTag oldView)) ->
    (newView : View) -> IO ()
  patch parentElement (Text { style = oldStyle } oldContent) oldElement (Text { style = newStyle, press } newContent) = do
    if newContent /= oldContent then oldElement.innerText_set newContent else pure ()
    oldElement.onclick_set $ \event => update press
    updateTextStyle oldElement (Just oldStyle) newStyle
  patch parentElement (Input {style = oldStyle} oldValue _) oldElement (Input {style = newStyle} newValue newChange) = do
    if newValue /= oldValue then oldElement.value_set newValue else pure ()
    oldElement.oninput_set $ \event => update (newChange !(!event.currentTarget).value)
    updateInputStyle oldElement (Just oldStyle) newStyle
  patch parentElement (Flex { style = oldStyle } oldChildren) oldElement (Flex { style = newStyle, press } newChildren) = do
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
    oldElement.onclick_set $ \event => update press
    updateFlexStyle oldElement (Just oldStyle) newStyle
  patch parentElement oldView oldElement newView =
    parentElement.replaceChild !(create newView) oldElement

namespace Root

  public export
  record Root where
    constructor MakeRoot
    element : Element "div"
    states : IORef $ SortedMap (List (String, String)) (identity : String ** Cell identity)
    views : IORef $ List View

  update : Root -> List View -> (updates : List StateUpdate) -> IO ()
  update root views updates = do
    let oldStates = !(readRef root.states)
    let newStates = foldl (\states => \(MakeStateUpdate path identity value) => insert path (identity ** MakeCell value) states) oldStates updates
    writeRef root.states newStates
    let oldViews = !(readRef root.views)
    let newViews = (flip mapi) views $ \index => \view => unfold empty newStates [("root", show index)] view
    writeRef root.views newViews
    patch (update root views) root.element (Flex oldViews) root.element (Flex newViews)

  export
  create : IO Root
  create = do
    document <- (!window).document
    element <- document.createElement "div"
    (!document.body).appendChild element
    let root = MakeRoot { element = element, states = !(newRef empty), views = !(newRef []) }
    pure root

  export
  (.render) : Root -> List View -> IO ()
  (.render) root views = update root views []
  