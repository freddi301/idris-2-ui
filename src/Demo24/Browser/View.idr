module Demo24.Browser.View

import Data.SortedMap.Dependent
import Data.SortedMap
import Data.Ref
import Demo24.Browser.DOM
import Demo24.View

viewToElementTag : View -> String
viewToElementTag (Text _) = "span"
viewToElementTag (Input _ _) = "input"
viewToElementTag (Flex _ _) = "div"
viewToElementTag _ = ?h2

parameters (update : List StateUpdate -> IO ())
  create : (view : View) -> IO (Element (viewToElementTag view))
  create (Text {press} content) = do
    element <- (!(!window).document).createElement "span"
    element.setInnerText content
    element.addEventListener "click" $ \event => update press
    pure element
  create (Flex direction children) = do
    element <- (!(!window).document).createElement "div"
    style <- element.style
    style.set "display" "flex"
    style.set "flexDirection" (case direction of Col => "column"; Row => "row")
    sequence_ $ map (\view => do element.appendChild !(create view)) children
    pure element
  create _ = ?h3


namespace Root

  data Root : Type where
    MakeRoot : (element : Element "div") -> (states : IORef $ SortedMap (List (String, String)) (identity : String ** Cell identity)) -> (children : List View) -> Root

  update : Root -> (updates : List StateUpdate) -> IO ()
  update root updates = do
    let (MakeRoot element states children) = root
    element.setInnerHTML ""
    oldStates <- readRef states
    let newStates = foldl (\states => \(MakeStateUpdate path identity value) => insert path (identity ** MakeCell value) states) oldStates updates
    writeRef states newStates
    sequence_ $ mapWithIndex (\(view, index) => do element.appendChild !(create (update root) (unfold empty newStates [("root", show index)] view))) children
    pure ()

  export
  create : List View -> IO Root
  create children = do
    document <- (!window).document
    element <- document.createElement "div"
    (!document.body).appendChild element
    states <- newRef empty
    let root = (MakeRoot element states children)
    update root []
    pure root
