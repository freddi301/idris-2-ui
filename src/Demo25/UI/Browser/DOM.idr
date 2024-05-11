module Demo25.UI.Browser.DOM

%foreign "javascript:lambda: (message) => { throw new Error(message); }"
prim__throw : String -> PrimIO ()

export
throw : String -> IO ()
throw message = primIO $ prim__throw message

%foreign "javascript:lambda: (object, attribute) => object[attribute]"
prim__getJSAttribute : AnyPtr -> String -> PrimIO AnyPtr

(.getJSAttribute) : AnyPtr -> String -> IO AnyPtr
(.getJSAttribute) object attribute = primIO $ prim__getJSAttribute object attribute

%foreign "javascript:lambda: (object, attribute, value) => object[attribute] = value"
prim__setJSAttribute : AnyPtr -> String -> AnyPtr -> PrimIO ()

(.setJSAttribute) : AnyPtr -> String -> AnyPtr -> IO ()
(.setJSAttribute) object attribute value = primIO $ prim__setJSAttribute object attribute value

-- TODO throw runtime error
interface RuntimeCast from to where
  cast : from -> IO to
  cast from = pure (believe_me from)

RuntimeCast AnyPtr String where
RuntimeCast String AnyPtr where

%foreign "browser:lambda: (object) => console.log(object)"
prim__console_log : AnyPtr -> PrimIO ()

namespace Console
  export
  log : anything -> IO ()
  log value = primIO $ prim__console_log (believe_me value)

data Window = MakeWindow AnyPtr

%foreign "browser:lambda: () => window"
prim__window : () -> PrimIO AnyPtr

export
window : IO Window
window = map MakeWindow $ primIO $ prim__window ()

data Document = MakeDocument AnyPtr

export
(.document) : Window -> IO Document
(.document) (MakeWindow window) = map MakeDocument $ window.getJSAttribute "document"

%foreign "browser:lambda: (document, type) => document.createElement(type)"
prim__createElement : AnyPtr -> String -> PrimIO AnyPtr

export
data Element : String -> Type where MakeElement : {tag : String} -> AnyPtr -> Element tag

interface ElementTag (tag : String) where

export ElementTag "body" where
export ElementTag "div" where
export ElementTag "span" where
export ElementTag "input" where

%foreign "browser:lambda: (element) => element.tagName.toLowerCase()"
prim__tagName : AnyPtr -> PrimIO String

(.tagName) : Element tag -> IO String
(.tagName) (MakeElement element) = primIO $ prim__tagName element

export
(.innerText_set) : Element anytag -> String -> IO ()
(.innerText_set) (MakeElement element) text = element.setJSAttribute "innerText" !(cast text)

export
(.innerHTML_set) : Element anytag -> String -> IO ()
(.innerHTML_set) (MakeElement element) text = element.setJSAttribute "innerHTML" !(cast text)

export
(.body) : Document -> IO (Element "body")
(.body) (MakeDocument window) = map MakeElement $ window.getJSAttribute "body"

export
(.createElement) : Document -> (tag : String) -> ElementTag tag => IO (Element tag)
(.createElement) (MakeDocument document) tag = map MakeElement $ primIO $ prim__createElement document tag

export
interface ElementTag tag => HasChildren tag where

export HasChildren "body" where
export HasChildren "div" where

%foreign "browser:lambda: (parent, child) => parent.appendChild(child)"
prim__appendChild : AnyPtr -> AnyPtr -> PrimIO ()

export
(.appendChild) : HasChildren tag => Element tag -> Element anytag -> IO ()
(.appendChild) (MakeElement parent) (MakeElement child) = primIO $ prim__appendChild parent child

%foreign "browser:lambda: (parent, child) => parent.removeChild(child)"
prim__removeChild : AnyPtr -> AnyPtr -> PrimIO ()

export
(.removeChild) : HasChildren tag => Element tag -> Element anytag -> IO ()
(.removeChild) (MakeElement parent) (MakeElement child) = primIO $ prim__removeChild parent child

%foreign "browser:lambda: (parent, newChild, oldChild) => parent.replaceChild(newChild, oldChild)"
prim__replaceChild : AnyPtr -> AnyPtr -> AnyPtr -> PrimIO ()

export
(.replaceChild) : HasChildren tag => Element tag -> Element anytag1 -> Element anytag2 -> IO ()
(.replaceChild) (MakeElement parent) (MakeElement newChild) (MakeElement oldChild) = primIO $ prim__replaceChild parent newChild oldChild

%foreign "browser:lambda: (parent) => __prim_js2idris_array(Array.from(parent.children))"
prim__children : AnyPtr -> PrimIO (List AnyPtr) 

export
(.children) : HasChildren tag => Element tag -> IO (List (tag : String ** Element tag))
(.children) (MakeElement parent) = pure $ map (\element => ((unsafePerformIO $ primIO $ prim__tagName element) ** MakeElement element)) !(primIO $ prim__children parent)

data Style = MakeStyle AnyPtr

export
(.style) : Element anytag -> IO Style
(.style) (MakeElement element) = map MakeStyle $ element.getJSAttribute "style"

export
(.get) : Style -> String -> IO String
(.get) (MakeStyle style) attribute = cast !(style.getJSAttribute attribute)

export
(.set) : Style -> String -> String -> IO ()
(.set) (MakeStyle style) attribute value = style.setJSAttribute attribute !(cast value)

data Event : String -> String -> Type where
  MakeEvent : {tag : String} -> {type : String} -> AnyPtr -> Event tag type

interface EventType (tag : String) (type : String) where

EventType "span" "click" where
EventType "input" "input" where

export
(.currentTarget) : Event "input" "input" -> IO (Element "input")
(.currentTarget) (MakeEvent event) = pure $ MakeElement !(event.getJSAttribute "currentTarget")

%foreign "browser:lambda: (node, event, callback) => node.addEventListener(event, x => callback(x)())"
prim__addEventListener : AnyPtr -> String -> (AnyPtr -> PrimIO ()) -> PrimIO ()

export
(.addEventListener) : Element tag -> (type : String) -> (Event tag type -> IO ()) -> IO ()
(.addEventListener) (MakeElement element) type callback =
  primIO $ prim__addEventListener element type (\event => toPrim $ callback (MakeEvent event))

%foreign "browser:lambda: (element, event, callback) => element[event] = x => callback(x)()"
prim__setAsEventListener : AnyPtr -> String -> (AnyPtr -> PrimIO ()) -> PrimIO ()

-- export
-- (.onclick) : Element tag -> IO (Event tag "click" -> IO ())
-- (.onclick) (MakeElement element) = map believe_me $ element.getJSAttribute "onclick"

export
(.onclick_set) : Element tag -> (Event tag "click" -> IO ()) -> IO ()
(.onclick_set) (MakeElement element) callback =
  primIO $ prim__setAsEventListener element "onclick" (\event => toPrim $ callback (MakeEvent event))

export
(.oninput_set) : Element tag -> (Event tag "input" -> IO ()) -> IO ()
(.oninput_set) (MakeElement element) callback =
  primIO $ prim__setAsEventListener element "oninput" (\event => toPrim $ callback (MakeEvent event))

export
(.value_set) : (Element "input") -> String -> IO ()
(.value_set) (MakeElement element) value = element.setJSAttribute "value" !(cast value)

export
(.value) : (Element "input") -> IO String
(.value) (MakeElement element) = cast !(element.getJSAttribute "value")

