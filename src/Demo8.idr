module Demo8

data Text : Type where
  Leaf : String -> Text
  Branch : List Text -> Text

%foreign "browser:lambda: () => document.body"
prim__documentBody : () -> PrimIO AnyPtr

documentBody : HasIO io => io AnyPtr
documentBody = primIO $ prim__documentBody ()

%foreign "browser:lambda: (type) => document.createElement(type)"
prim__createElement : String -> PrimIO AnyPtr

createElement : HasIO io => String -> io AnyPtr
createElement type = primIO $ prim__createElement type

%foreign "browser:lambda: (span, text) => span.innerText = text"
prim__setSpanText : AnyPtr -> String -> PrimIO ()

setSpanText : HasIO io => AnyPtr -> String -> io ()
setSpanText span text = primIO $ prim__setSpanText span text

%foreign "browser:lambda: (parent, child) => parent.appendChild(child)"
prim__appendChild : AnyPtr -> AnyPtr -> PrimIO ()

appendChild : HasIO io => AnyPtr -> AnyPtr -> io ()
appendChild parent child = primIO $ prim__appendChild parent child

create : HasIO io => Text -> io AnyPtr
create (Leaf text) = do
  element <- createElement "span"
  setSpanText element text
  pure element
create (Branch children) = doIt where
  appendChildren : AnyPtr -> List Text -> io ()
  appendChildren parent [] = pure ()
  appendChildren parent (head :: tail) = do
    child <- create head
    appendChild parent child
    appendChildren parent tail
  doIt = do
    element <- createElement "span"
    appendChildren element children
    pure element
  

main : IO ()
main = do
  content <- create (Branch [Leaf "Hello", Leaf "World"])
  body <- documentBody
  appendChild body content
  pure ()