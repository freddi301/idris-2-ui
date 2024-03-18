module Demo18.Dsl

import public Demo18.Data

[emptyBaseEventHandlers] DataBaseEventHandlers where

export
Text : {default emptyBaseEventHandlers on : DataBaseEventHandlers} -> (content : String) -> DataView
Text content = DataText {content = content, eventHandlers = on}

export
Input : {value : String} -> {change : String -> Lazy DataView} -> DataView
Input = DataInput {value = value, change = change} 

export
Layout : {default emptyBaseEventHandlers on : DataBaseEventHandlers} -> (children : List DataView) -> DataView
Layout children = DataLayout {children = children, eventHandlers = on}

public export
interface Component state where
  initial : state
  render : state -> (update : state -> DataView) -> DataView

export
Instance : (component : Component state) -> DataView
Instance component = DataInstance (initial @{component}) (render @{component})

export
event : ?todo2
event = MakeDataBaseEventHandlers