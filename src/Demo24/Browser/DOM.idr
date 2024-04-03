module Demo24.Browser.DOM

%foreign "browser:lambda: (object, attribute) => object[attribute]"
prim__getJSAttribute : AnyPtr -> String -> PrimIO AnyPtr

(.getJSAttribute) : AnyPtr -> String -> IO AnyPtr
(.getJSAttribute) object attribute = primIO $ prim__getJSAttribute object attribute

%foreign "browser:lambda: (object, attribute, value) => object[attribute] = value"
prim__setJSAttribute : AnyPtr -> String -> AnyPtr -> PrimIO ()

(.setJSAttribute) : AnyPtr -> String -> AnyPtr -> IO ()
(.setJSAttribute) object attribute value = primIO $ prim__setJSAttribute object attribute value

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

export
(.setInnerText) : Element anytag -> String -> IO ()
(.setInnerText) (MakeElement element) text = element.setJSAttribute "innerText" (believe_me text)

export
(.setInnerHTML) : Element anytag -> String -> IO ()
(.setInnerHTML) (MakeElement element) text = element.setJSAttribute "innerHTML" (believe_me text)

export
(.body) : Document -> IO (Element "body")
(.body) (MakeDocument window) = map MakeElement $ window.getJSAttribute "body"

export
(.createElement) : Document -> (tag : String) -> ElementTag tag => IO (Element tag)
(.createElement) (MakeDocument document) tag = map MakeElement $ primIO $ prim__createElement document tag

%foreign "browser:lambda: (parent, child) => parent.appendChild(child)"
prim__appendChild : AnyPtr -> AnyPtr -> PrimIO ()

interface ElementTag tag => HasChildren tag where

export HasChildren "body" where
export HasChildren "div" where

export
(.appendChild) : HasChildren tag => Element tag -> Element anytag -> IO ()
(.appendChild) (MakeElement parent) (MakeElement child) = primIO $ prim__appendChild parent child

data Style = MakeStyle AnyPtr

export
(.style) : Element anytag -> IO Style
(.style) (MakeElement element) = map MakeStyle $ element.getJSAttribute "style"

export
(.get) : Style -> String -> IO String
(.get) (MakeStyle style) attribute = map believe_me $ style.getJSAttribute attribute

export
(.set) : Style -> String -> String -> IO ()
(.set) (MakeStyle style) attribute value = style.setJSAttribute attribute (believe_me value)

data Event : String -> String -> Type where MakeEvent : {tag : String} -> {type : String} -> AnyPtr -> Event tag type

interface EventType (tag : String) (type : String) where

EventType "span" "click" where

%foreign "browser:lambda: (node, event, callback) => node.addEventListener(event, x=>callback(x)())"
prim__addEventListener : AnyPtr -> String -> (AnyPtr -> PrimIO ()) -> PrimIO ()

export
(.addEventListener) : Element tag -> (type : String) -> (Event tag type -> IO ()) -> IO ()
(.addEventListener) (MakeElement element) type callback =
  primIO $ prim__addEventListener element type (\event => toPrim $ callback (MakeEvent event))
