module Demo16.Dsl

import public Demo16.Data as Data

[emptyBaseEventHandlers] BaseEventHandlers where

export
Text : {default emptyBaseEventHandlers on : BaseEventHandlers} -> (content : String) -> Data.View
Text content = Data.Text {content = content, eventHandlers = on}

export
Input : {value : String} -> {change : String -> Lazy Data.View} -> Data.View
Input = Data.Input {value = value, change = change} 

export
Layout : {default emptyBaseEventHandlers on : BaseEventHandlers} -> (children : List Data.View) -> Data.View
Layout children = Data.Layout {children = children, eventHandlers = on}

export
event : ?todo2
event = MakeBaseEventHandlers