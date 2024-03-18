module Demo21.Browser

import Demo21.Dsl

%foreign "browser:lambda: () => window"
prim__window : () -> PrimIO AnyPtr

window : IO AnyPtr
window = primIO $ prim__window ()

%foreign "browser:lambda: (type) => document.createElement(type)"
prim__createElement : String -> PrimIO AnyPtr

createElement : String -> IO AnyPtr
createElement type = primIO $ prim__createElement type

%foreign "browser:lambda: (parent, child) => parent.appendChild(child)"
prim__appendChild : AnyPtr -> AnyPtr -> PrimIO ()

appendChild : AnyPtr -> AnyPtr -> IO ()
appendChild parent child = primIO $ prim__appendChild parent child

%foreign "browser:lambda: (node, event, callback) => node.addEventListener(event, x=>callback(x)())"
prim__addEventListener : AnyPtr -> String -> (AnyPtr -> PrimIO ()) -> PrimIO ()

addEventListener : AnyPtr -> String -> (AnyPtr -> IO ()) -> IO ()
addEventListener node event callback =
  primIO $ prim__addEventListener node event (\ptr => toPrim $ callback ptr)

%foreign "browser:lambda: (object, attribute) => object[attribute]"
prim__getAttribute : AnyPtr -> String -> PrimIO AnyPtr

(.getAttribute) : AnyPtr -> String -> IO AnyPtr
(.getAttribute) object attribute = primIO $ prim__getAttribute object attribute

%foreign "browser:lambda: (object, attribute, value) => object[attribute] = value"
prim__setAttribute : AnyPtr -> String -> AnyPtr -> PrimIO ()

(.setAttribute) : AnyPtr -> String -> AnyPtr -> IO ()
(.setAttribute) object attribute value = primIO $ prim__setAttribute object attribute value

Cast String AnyPtr where
  cast string = believe_me string 

---

create : (View -> IO ()) -> View -> IO AnyPtr
create update view = element view where
  appendChildren : AnyPtr -> List View -> IO ()
  appendChildren parent [] = pure ()
  appendChildren parent (head :: tail) = do
    child <- create update head
    appendChild parent child
    appendChildren parent tail
  addListener : AnyPtr -> String -> (AnyPtr -> IO $ Lazy View) -> IO ()
  addListener parent event next = addEventListener parent event (\e => do nextView <- next e; updateIfNecessary nextView) where
    updateIfNecessary : View -> IO ()
    updateIfNecessary DoNotUpdate = pure ()
    updateIfNecessary next = update next
  element : View -> IO AnyPtr
  element DoNotUpdate = do
    parent <- createElement "div"
    parent.setAttribute "innerText" (cast "DO NOT UPDATE") 
    -- TODO
    pure parent
  element (Text {event} content) = do
    parent <- createElement "div"
    addListener parent "click" $ \_ => pure $ press @{event}
    parent.setAttribute "innerText" (cast content) 
    pure parent
  element (Input value change) = do
    parent <- createElement "input"
    parent.setAttribute "value" (cast value) 
    addListener parent "input" $ \event => do
      value <- (!(event.getAttribute "currentTarget")).getAttribute "value"
      pure $ change (believe_me value)
    pure parent
  element (Layout direction {event} children) = do
    parent <- createElement "div"
    style <- parent.getAttribute "style"
    style.setAttribute "display" (cast "flex")
    case direction of
      Col => style.setAttribute "flexDirection" (cast "column")
      Row => style.setAttribute "flexDirection" (cast "row")
    addListener parent "click" $ \_ => pure $ press @{event}
    appendChildren parent children
    pure parent

update : AnyPtr -> View -> IO ()
update root next = do
  content <- create (update root) next
  root.setAttribute "innerHTML" (cast "")
  appendChild root content

render : View -> IO ()
render view = do
  root <- createElement "div"
  appendChild !((!((!window).getAttribute "document")).getAttribute "body") root
  update root view

export
renderRoot : Component shape -> IO ()
renderRoot instance = render $ mapper (runInit instance) where
  mapper state = runNext instance [] state mapper