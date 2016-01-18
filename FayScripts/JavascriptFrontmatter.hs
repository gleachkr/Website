---vim:fdm=marker
{-# LANGUAGE EmptyDataDecls #-}

module JavascriptFrontmatter where 

import Fay.FFI
import Prelude

data Event

data Element 
instance Show Element

data Ref a

alert :: String -> Fay ()
alert = ffi "alert(%1)"

scrollTo :: Int -> Int -> Fay ()
scrollTo = ffi "window.scrollTo(%1,%2)"

setBodyHtml :: String -> Fay ()
setBodyHtml = ffi "document.body.innerHTML = %1"

--make a new mutable reference
newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

--replace the contents of a given mutable reference
writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

--read the contents of a given mutable reference
readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

newElement :: String -> Fay Element
newElement = ffi "document['createElement'](%1)"

bodyHtml :: Fay String
bodyHtml = ffi "document.body.innerHTML"

setElementHtml :: Element -> String -> Fay ()
setElementHtml = ffi "%1.innerHTML = %2"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

getElementById :: String -> Fay Element
getElementById = ffi "document['getElementById'](%1)"

getElementsByClass :: String -> Fay [Element]
getElementsByClass = ffi "document['getElementsByClassName'](%1)"

getChildren :: Element -> Fay [Element]
getChildren = ffi "%1.children"

setElementClass :: Element -> String -> Fay ()
setElementClass = ffi "%1.className = %2"

getSubElementsByClass :: Element -> String -> Fay [Element]
getSubElementsByClass = ffi "%1['getElementsByClassName'](%2)"

classOfElement :: Element -> Fay String
classOfElement = ffi "%1.className"

idOfElement :: Element -> Fay String
idOfElement = ffi "%1.id"

getSubElementsByTag :: Element -> String -> Fay [Element]
getSubElementsByTag = ffi "%1['getElementsByTagName'](%2)"


----------Event_Listeners {{{

addEventListener :: Element -> String -> (Event -> Fay Bool) -> Fay ()
addEventListener = ffi "%1['addEventListener'](%2,%3,false)"

removeEventListener :: Element -> String -> (Event -> Fay Bool) -> Fay ()
removeEventListener = ffi "$1['removeEventListener'](%2,%3)"

--}}}

--makes element 2 the last child of element 1
appendChild :: Element -> Element -> Fay ()
appendChild = ffi "%1.appendChild(%2)"

deleteNode :: Element -> Fay ()
deleteNode = ffi "%1.parentNode.removeChild(%1)"

--inserts element 2 as a sibling below element 1
insertAfter :: Element -> Element -> Fay ()
insertAfter = ffi "%1.parentNode.insertBefore(%2,%1.nextSibling)"

--gets the parent element of a given element
parentNode :: Element -> Fay Element
parentNode = ffi "%1.parentNode"

--Clones an HTML Element, together with its descendents
toClone :: Element -> Fay Element
toClone = ffi "%1.cloneNode(true)"

--inserts element 2 as a sibling above element 1
insertBefore :: Element -> Element -> Fay ()
insertBefore = ffi "%1.parentNode.insertBefore(%2,%1)"
--on this approch, event handlers have the type Event -> Fay Bool

makeMutable :: Element -> Fay ()
makeMutable = ffi "%1.setAttribute('contentEditable','true')"

----------Drag_And_Drop{{{

makeDraggable :: Element -> Fay ()
makeDraggable = ffi "%1.setAttribute('draggable','true')"

setDragString :: Event -> String -> Fay ()
setDragString = ffi "%1.dataTransfer.setData('text/plain',%2)"

interceptDrop :: Event -> Fay ()
interceptDrop = ffi "%1.preventDefault()"

unwrapDropString :: Event -> Fay String
unwrapDropString = ffi "%1.dataTransfer.getData('text/plain')"

--}}}
----------Class_Manipulation{{{

addClass :: String -> Element -> Fay ()
addClass = ffi "%2.classList.add(%1)"

removeClass :: String -> Element -> Fay ()
removeClass = ffi "%2.classList.remove(%1)"

hasClass :: String -> Element -> Fay Bool
hasClass = ffi "%2.classList.contains(%1)"

---}}}
