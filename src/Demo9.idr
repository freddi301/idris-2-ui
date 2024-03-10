module Demo9

%foreign "browser:lambda: () => document.body"
prim__documentBody : () -> PrimIO AnyPtr

documentBody : IO AnyPtr
documentBody = primIO $ prim__documentBody ()

%foreign "browser:lambda: (type) => document.createElement(type)"
prim__createElement : String -> PrimIO AnyPtr

createElement : String -> IO AnyPtr
createElement type = primIO $ prim__createElement type

%foreign "browser:lambda: (element, text) => element.innerText = text"
prim__setInnerText : AnyPtr -> String -> PrimIO ()

setInnerText : AnyPtr -> String -> IO ()
setInnerText element text = primIO $ prim__setInnerText element text

%foreign "browser:lambda: (parent, child) => parent.appendChild(child)"
prim__appendChild : AnyPtr -> AnyPtr -> PrimIO ()

appendChild : AnyPtr -> AnyPtr -> IO ()
appendChild parent child = primIO $ prim__appendChild parent child

%foreign "browser:lambda: (node, event, callback) => node.addEventListener(event, x=>callback(x)())"
prim__addEventListener : AnyPtr -> String -> (AnyPtr -> PrimIO ()) -> PrimIO ()

addEventListener : AnyPtr -> String -> (AnyPtr -> IO ()) -> IO ()
addEventListener node event callback =
  primIO $ prim__addEventListener node event (\ptr => toPrim $ callback ptr)

---

data Text : Type where
  Leaf : {default Nothing onClick : Maybe $ () -> Text} -> String -> Text
  Branch : {default Nothing onClick : Maybe $ () -> Text} -> List Text -> Text

create : (Text -> IO ()) -> Text -> IO AnyPtr
create update text = element text where
  appendChildren : AnyPtr -> List Text -> IO ()
  appendChildren parent [] = pure ()
  appendChildren parent (head :: tail) = do
    child <- create update head
    appendChild parent child
    appendChildren parent tail
  addListener : AnyPtr -> Maybe (() -> Text) -> IO ()
  addListener parent Nothing = pure ()
  addListener parent (Just next) = addEventListener parent "click" (\_ => update $ next ())
  element : Text -> IO AnyPtr
  element (Leaf {onClick} text) = do
    parent <- createElement "span"
    addListener parent onClick
    setInnerText parent text
    pure parent
  element (Branch {onClick} children) = do
    parent <- createElement "span"
    addListener parent onClick
    appendChildren parent children
    pure parent

update : Text -> IO ()
update next = do
  content <- create update next
  body <- documentBody
  appendChild body content
  pure ()

main : IO ()
main = do
  update (Branch [
    Leaf "Hello",
    Leaf "World",
    Leaf {onClick = Just \_ => Leaf "Clicked"} "Click"
  ])
