module Demo16.Data

mutual

  public export
  data View : Type where
    DoNotUpdate : View
    Text : (content : String) -> (eventHandlers : BaseEventHandlers) -> View
    Input : (value : String) -> (change : String -> Lazy View) -> View
    Layout : (children : List View) -> (eventHandlers : BaseEventHandlers) -> View

  export
  doNotUpdate : View
  doNotUpdate = DoNotUpdate

  public export
  interface BaseEventHandlers where
    constructor MakeBaseEventHandlers
    press : Lazy View
    press = doNotUpdate
