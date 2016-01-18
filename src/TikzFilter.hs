{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TikzFilter (tikzBlock) where
import Shelly
import Data.Text
import Data.Monoid
import System.IO.Unsafe
import Text.Pandoc

tikzBlock :: String -> Block -> Block
tikzBlock context (CodeBlock (id , classes, _ ) code) 
    | classes == ["latex","tikz"] = tikzToSVG (pack code) (pack id <> "_" <> pack context)
tikzBlock context x = x

htmlWrap :: String -> String
htmlWrap name = "<object class=\"tikzImage\" type=\"image/svg+xml\" data=\"/images/" ++ name ++ "\"> Trouble Loading an SVG image here...</object>"

texTemplate :: Text -> Text
texTemplate tikz = "\\nonstopmode"    <>
   "\\documentclass[18pt, varwidth=4in]{standalone}\n" <>
                  "\\usepackage{tikz}\n" <>
            "\\usetikzlibrary{arrows}\n" <>
       "\\usetikzlibrary{positioning}\n" <>
            "\\usetikzlibrary{snakes}\n" <>
            "\\usepackage{tikz-qtree}\n" <>
            "\\usepackage{bussproofs}\n" <>
                "\\usepackage{KMcalc}\n" <>
              "\\usepackage{arydshln}\n" <>
                   "\\begin{document}\n" <>
                    "\\parbox{4in}{" <> tikz <> "}" <>
                     "\\end{document}\n"

processTikzInTmp :: Text -> Text -> Text -> Shelly.FilePath -> Sh ()
processTikzInTmp code id cleanName path = do
        mkdir_p "_site/images/"
        alreadyExists <- test_e (fromText $ "_site/images/" <> cleanName)
        if alreadyExists 
            then 
                echo $ "_site/images" <> cleanName <> " already exists"
            else do 
                origin <- pwd
                cd path
                writefile "tikz.tex" $ texTemplate code
                run_ "pdflatex" ["tikz.tex"]
                run_ "pdf2svg" ["tikz.pdf", cleanName]
                mv (fromText cleanName) $ origin </> "_site/images/"

--add a check to see if a recent version of the file already exists.
tikzToSVG :: Text -> Text -> Block
tikzToSVG code id = unsafePerformIO $ do
        let cleanName = Data.Text.map cleanspecials id <> ".svg"        
        shelly $ silently $ withTmpDir (processTikzInTmp code id cleanName)
        let nameString = unpack cleanName
        return $ RawBlock (Format "html") (htmlWrap nameString)

cleanspecials x  
        | x == '/' = '_'
        | x == '.' = '_'
        | True = x
