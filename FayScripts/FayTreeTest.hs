{-# LANGUAGE EmptyDataDecls #-}
module FayTreeTest where

import Fay.FFI
import JavascriptFrontmatter

neckCheck :: Element -> Fay Bool
neckCheck element = parentNode element >>= (\parent -> getSubElementsByClass parent "Neck") >>=  (\x -> return (not (null x)))

headCheck :: Element -> Fay Bool
headCheck element = (getSubElementsByTag element "li") >>= (\x -> return (not (null x)))

--adds the event listener to make a head a hydra-head
hydrafy :: Ref Int -> Element -> Fay ()
hydrafy battleStage e = addEventListener e "click" (hydrate battleStage e)

hydrate :: Ref Int -> Element -> Event -> Fay Bool
hydrate battleStage element event = do
        s <- readRef battleStage
        isTerminal <- neckCheck element
        if s == 0 then growHead battleStage element event
                  else if isTerminal then return False
                                     else splitHead battleStage element event

nParentNode :: Int -> Element -> Fay Element
nParentNode n x = iterate (\y-> y >>= parentNode) (return x) !! n

growHead :: Ref Int -> Element -> Event -> Fay Bool
growHead battleStage element event = do
        hasNeck <- neckCheck element
        if hasNeck then newHead battleStage element event
                   else do 
                           newNeck element 
                           newHead battleStage element event

newNeck element = do
        x <- newElement "ul"
        setElementClass x "Neck"
        insertAfter element x

chop :: Element -> Fay ()
chop element = do
        neck <- parentNode element
        parentNeck <- nParentNode 2 element
        deleteNode neck
        headsRemain <- headCheck parentNeck
        if headsRemain then return ()
                       else deleteNode parentNeck
                           

splitHead :: Ref Int -> Element -> Event -> Fay Bool
splitHead battleStage element event = do
        n <- readRef battleStage
        writeRef battleStage (n+1::Int)
        ggParent <- nParentNode 3 element 
        gggParent <- nParentNode 1 ggParent
        gggClass <- classOfElement gggParent
        chop element
        let seeds = take n [1..]
        if gggClass == "Neck" 
            then do 
                    clones <- mapM (\_-> toClone ggParent) seeds 
                    clonedHeadBundles <- mapM (\x -> getSubElementsByClass x "Head") clones
                    let clonedHeads = concat clonedHeadBundles
                    mapM_ (hydrafy battleStage) clonedHeads
                    mapM_ (appendChild gggParent) clones 
                    return True
            else do 
                    return True


newHead :: Ref Int -> Element -> Event -> Fay Bool
newHead battleStage element event = do
        x <- newElement "li"
        makeMutable x
        setElementHtml x "<div class='Head'></div>"
        theHeads <- getSubElementsByTag x "div"
        hydrafy battleStage (head theHeads)
        parent <- parentNode element
        uls <- getSubElementsByTag parent "ul"
        ul <- return (head uls)
        appendChild ul x
        return True

main :: Fay ()
main = addWindowEvent "load" run
    
run event = do
        x <- getElementsByClass "Head"
        b <- getElementById "theButton"
        battleStage <- newRef (0::Int)
        battleButtonize battleStage b
        mapM_ (hydrafy battleStage) x 

battleButtonize :: Ref Int -> Element -> Fay ()
battleButtonize battleStage b = addEventListener b "click" (initializeBattle battleStage)

initializeBattle battleStage event = do
        writeRef battleStage (1::Int)
        return True
