{-# LANGUAGE EmptyDataDecls #-}
module CartargrapherClone where

import Fay.FFI
import JavascriptFrontmatter

--the Html trees being manipulated consist of an unordered list, whose
--items consist of 
--1. a div, which is either a Node (nonterminal) or a Leaf (terminal)
--2. if a Node, also a ul (the "neck" extending from the node), whose list
--items are as in the main list.
--So, these are essentially rose trees.

--checks to see whether a given <div>--leaf or node---has an associated
--"Neck" (is nontermial)
neckCheck :: Element -> Fay Bool
neckCheck element = parentNode element 
        >>= (\parent -> getSubElementsByClass parent "Neck") 
        >>= (\x -> return (not (null x)))

--returns True for leaves/nodes extending from something supporting "thesis"
parentThesis :: Element -> Fay Bool
parentThesis element = do
        greatGrandParent <- nParentNode 3 element --should be the li one level up.
        children <- getChildren greatGrandParent
        hasClass "thesis" $ head children 

--nothing obvious to do to initialize roots.
rootify :: Element -> Fay ()
rootify e = do
        addClass "thesis"
        return ()

--makes moves carry their Ids as data upon drag.
movify :: Element -> Fay ()
movify e = do 
        makeDraggable e
        theId <- idOfElement e
        addEventListener e "dragstart" $ dragIt theId
        
--adds event listener to make leaves bloom on drop.
leafify :: Element -> Fay ()
leafify e = do 
        makeMutable e
        addEventListener e "drop" (bloom e)
        addEventListener e "dragenter" (highlight e)
        addEventListener e "dragleave" (unhighlight e)

--the code to turn a leaf into a node on click or drop
--upacks a drop event to get the relevant maturation
mature :: Element -> Event -> Fay Bool
mature element event = do
        addClass "Node" element
        removeClass "Leaf" element
        moveType <- unwrapDropString event
        addClass ("with_" ++ moveType) element
        fromThesis <- parentThesis element
        if moveType == "objection"
            then if fromThesis 
                     then addClass "antithesis" element
                     else addClass "thesis" element
            else if fromThesis
                     then addClass "thesis" element
                     else addClass "antithesis" element
        setElementHtml element "<textarea></textarea>"
        growLeaf element
        parent <- nParentNode 2 element 
        growLeaf parent
        addClass "Grand" parent
        return True


--clean up css effect, handle the events, and if the element is a Leaf,
--mature it.
bloom :: Element -> Event -> Fay Bool
bloom element event = do
        removeClass "dropOver" element 
        interceptDrop event
        hasLeaf <- hasClass "Leaf" element
        if hasLeaf then mature element event
                   else return False

growLeaf :: Element -> Fay Bool
growLeaf element = do
        hasNeck <- neckCheck element
        if hasNeck then newLeaf element 
                   else do newNeck element 
                           newLeaf element 

nParentNode :: Int -> Element -> Fay Element
nParentNode n x = iterate (\y-> y >>= parentNode) (return x) !! n
--climbs the DOM tree n steps, and returns the corresponding parent node.

newNeck element = do
        x <- newElement "ul"
        setElementClass x "Neck"
        insertAfter element x

newLeaf :: Element -> Fay Bool
newLeaf element = do
        x <- newElement "li"
        makeMutable x
        setElementHtml x "<div class='Leaf'>+</div>"
        theHeads <- getSubElementsByTag x "div"
        leafify (head theHeads)
        parent <- parentNode element
        uls <- getSubElementsByTag parent "ul"
        ul <- return (head uls)
        appendChild ul x
        return True

main :: Fay ()
main = addWindowEvent "load" run

dragIt :: String -> Event -> Fay Bool
dragIt message event = do 
        setDragString event message
        return True

dropIt :: Event -> Fay Bool
dropIt event = do
        interceptDrop event
        interceptedString <- unwrapDropString event
        alert interceptedString
    
highlight :: Element -> Event -> Fay Bool
highlight element event = do
        interceptDrop event
        addClass "dropOver" element 
        return True

unhighlight :: Element -> Event -> Fay Bool
unhighlight element event = do
        interceptDrop event
        removeClass "dropOver" element 
        return True

run event = do
        x <- getElementsByClass "Root"
        y <- getElementsByClass "Leaf"
        z <- getElementsByClass "Move"
        mapM_ rootify x
        mapM_ leafify y
        mapM_ movify z
        scrollTo 1500 0
