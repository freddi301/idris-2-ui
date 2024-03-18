module Demo18.Browser

import Demo18.Data as Data

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

%foreign "browser:lambda: (element, text) => element.innerHTML = text"
prim__setInnerHTML : AnyPtr -> String -> PrimIO ()

setInnerHTML : AnyPtr -> String -> IO ()
setInnerHTML element text = primIO $ prim__setInnerHTML element text

%foreign "browser:lambda: (parent, child) => parent.appendChild(child)"
prim__appendChild : AnyPtr -> AnyPtr -> PrimIO ()

appendChild : AnyPtr -> AnyPtr -> IO ()
appendChild parent child = primIO $ prim__appendChild parent child

%foreign "browser:lambda: (node, event, callback) => node.addEventListener(event, x=>callback(x)())"
prim__addEventListener : AnyPtr -> String -> (AnyPtr -> PrimIO ()) -> PrimIO ()

addEventListener : AnyPtr -> String -> (AnyPtr -> IO ()) -> IO ()
addEventListener node event callback =
  primIO $ prim__addEventListener node event (\ptr => toPrim $ callback ptr)


%foreign "browser:lambda: (element, text) => element.value = text"
prim__setValue : AnyPtr -> String -> PrimIO ()

setValue : AnyPtr -> String -> IO ()
setValue element text = primIO $ prim__setValue element text

%foreign "browser:lambda: (object, attribute) => object[attribute]"
prim__getAttribute : AnyPtr -> String -> PrimIO AnyPtr

getAttribute : AnyPtr -> String -> IO AnyPtr
getAttribute element text = primIO $ prim__getAttribute element text

---

create : (DataView -> IO ()) -> DataView -> IO AnyPtr
create update view = element view where
  appendChildren : AnyPtr -> List DataView -> IO ()
  appendChildren parent [] = pure ()
  appendChildren parent (head :: tail) = do
    child <- create update head
    appendChild parent child
    appendChildren parent tail
  addListener : AnyPtr -> String -> (AnyPtr -> IO $ Lazy DataView) -> IO ()
  addListener parent event next = addEventListener parent event (\e => do nextView <- next e; updateIfNecessary nextView) where
    updateIfNecessary : DataView -> IO ()
    updateIfNecessary DoNotUpdate = pure ()
    updateIfNecessary next = update next
  element : DataView -> IO AnyPtr
  element DoNotUpdate = do
    parent <- createElement "div"
    (setInnerText parent "DO NOT UPDATE")
    -- TODO
    pure parent
  element (DataInstance state render) = do
    parent <- createElement "div"
    (setInnerText parent "INTANCE")
    -- TODO
    pure parent
  element (DataText content eventHandlers) = do
    parent <- createElement "div"
    addListener parent "click" $ \_ => pure $ press @{eventHandlers}
    setInnerText parent content
    pure parent
  element (DataInput value change) = do
    parent <- createElement "input"
    setValue parent value
    addListener parent "input" $ \e => do
      currentTarget <- (getAttribute e "currentTarget")
      value <- (getAttribute currentTarget "value")
      putStrLn (believe_me value)
      pure $ change (believe_me value)
    pure parent
  element (DataLayout children eventHandlers) = do
    parent <- createElement "div"
    addListener parent "click" $ \_ => pure $ press @{eventHandlers}
    appendChildren parent children
    pure parent

update : AnyPtr -> DataView -> IO ()
update root next = do
  content <- create (update root) next
  setInnerHTML root ""
  appendChild root content

export
render : DataView -> IO ()
render view = do
  body <- documentBody
  root <- createElement "div"
  appendChild body root
  update root (renderDataView id view)