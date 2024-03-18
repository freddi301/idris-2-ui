module Demo18.Data

mutual

  public export
  data DataView : Type where
    DoNotUpdate : DataView
    DataText : (content : String) -> (eventHandlers : DataBaseEventHandlers) -> DataView
    DataInput : (value : String) -> (change : String -> Lazy DataView) -> DataView
    DataLayout : (children : List DataView) -> (eventHandlers : DataBaseEventHandlers) -> DataView
    DataInstance : state -> (render : state -> (update : state -> DataView) -> DataView) -> DataView

  export
  doNotUpdate : DataView
  doNotUpdate = DoNotUpdate

  public export
  interface DataBaseEventHandlers where
    constructor MakeDataBaseEventHandlers
    press : Lazy DataView
    press = doNotUpdate

orElse : Maybe a -> a -> a
orElse Nothing def = def
orElse (Just val) def = val

append : List a -> a -> List a
append [] v = [v]
append (h :: t) v = h :: (append t v)

export
renderDataView : (DataView -> DataView) -> DataView -> DataView
renderDataView parent DoNotUpdate = DoNotUpdate
renderDataView parent (DataText content eventHandlers) = (DataText content eventHandlers)
renderDataView parent (DataInput value change) = (DataInput value change)
renderDataView parent (DataLayout children eventHandlers) = (DataLayout (renderDataLayout [] children) eventHandlers) where
  renderDataLayout : List DataView -> List DataView -> List DataView
  renderDataLayout prev [] = prev
  renderDataLayout prev (h :: t) = renderDataLayout (append prev (renderDataView branchParent h)) t where
    branchParent child = parent (DataLayout ((append prev child) ++ (renderDataLayout (append prev child) t)) eventHandlers)
renderDataView parent (DataInstance state render) = render state update where
  update state = parent $ renderDataView parent (DataInstance state render)