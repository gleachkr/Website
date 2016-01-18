{-# LANGUAGE OverloadedStrings #-}
import           Prelude 
import           Data.Monoid (mempty, mappend)
import           Data.Functor
import           Data.Time.Clock 
import           Data.Time.Calendar
import           Control.Monad (filterM)
import           System.FilePath (takeDirectory)
import           System.Locale (defaultTimeLocale)
import           Hakyll
import           Text.Pandoc
import           Text.Pandoc.Walk
import           Fay as F
import           TikzFilter
import           Data.Map as M (lookup)
import           Data.Set as S (singleton, union)

--------------------------------------------------------------------------------
theConfiguration :: Configuration
theConfiguration = defaultConfiguration 
    {deployCommand = "s3cmd sync ~/Sites/MySite/_site/ s3://grahamlk.net"}

main :: IO ()
main = hakyllWith theConfiguration $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "resources/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "resources/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "resources/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    --these are loaded later by the front page
    match "components/*" $ compile getResourceBody
    match "papers/*" $ compile getResourceBody
    match "presentations/*" $ compile getResourceBody

    --compile the body of a given set of notes, and make it into the pandoc
    --version of that resource. The double-star pattern means I can
    --locally organize the notes however I'd like.
    --
    --the reason they have a route is to make sure that their URL gets into
    --their metadata, so that when we do the "for" loop in generating the
    --table, the urls get in. There must be a more correct way of doing
    --this.
    match "notes/**/*.md" $ version "pandoc" $ do
        route $ metadataRoute noteRoute
        compile bodyPandocCompiler

    --compile a toc for a given set of notes or syllabus, and make it into the toc
    --version of that resource. 
    match ("notes/**/*.md" .||. "syllabi/*.md") $ version "toc" $ compile tocPandocCompiler

    --deploy note body and toc along an appropriate route.
    match "notes/**/*.md" $ do
        route $ metadataRoute noteRoute
        compile noteCompiler

    match "syllabi/*" $ compile bodyPandocCompiler

    --compile a main page for each course
    match "courses/*" $ do
        route $ metadataRoute $ courseRoute "index.html"
        compile courseCompiler

    --compile a syllabus for each course
    match "courses/*" $ version "syllabus" $ do
        route $ metadataRoute $ courseRoute "Syllabus.html"
        compile syllabusCompiler     

    match "geekery/*.html" $ do
        route idRoute
        compile geekPageCompiler

    match "index.html" $ do
        route idRoute
        compile frontPageCompiler

    match "FayScripts/*.hs" $ do
        route $ composeRoutes (setExtension  "js") (gsubRoute "FayScripts" (const "scripts"))
        compile $ fayToJsCompiler >>= loadAndApplyTemplate "templates/beer.js" defaultContext

    match "templates/*" $ compile templateCompiler

--Compilers---------------------------------------------------------------------

--compiles the salient pandoc resource, with TikzBlocks
bodyPandocCompiler = do 
        id <- getUnderlying
        let pathText = toFilePath id
        writePandocWith defaultHakyllWriterOptions 
            <$> fmap (walk (tikzBlock pathText))
            <$> readPandocWith defaultHakyllReaderOptions
                { readerExtensions = S.union pandocExtensions $ S.singleton Ext_tex_math_double_backslash }
            <$> getResourceBody

--generates a TOC for the salient pandoc resource
tocPandocCompiler =  pandocCompilerWith defaultHakyllReaderOptions 
        (defaultHakyllWriterOptions { writerTableOfContents = True, 
        --generate a toc
        writerTemplate = "$toc$",      
        --in Standalone mode, compile into a template that only includes the toc
        writerStandalone = True 
        }) 

--applies the geekTemplate to the body of the salient resource
geekPageCompiler = getResourceBody >>= loadAndApplyTemplate "templates/geekTemplate.html" defaultContext
                                   >>= relativizeUrls

--runs the salient resource through fay
fayToJsCompiler :: Compiler (Item String)
fayToJsCompiler = do 
        path   <- getResourceFilePath
        let dir = takeDirectory path
        let localConfig = defaultConfig {
                configPackageConf = Just "/Users/Graham/Sites/MySite/.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d/"}
        let localConfig' = addConfigDirectoryIncludePaths [dir] localConfig
        fayOut <- unsafeCompiler (compileFile localConfig' path)
        makeItem (mergeCases fayOut)

--returns a list of the titles of notes, in chronological order, from the
--same course as the salient resource
loadSortedMatchingCompiledNotes :: Compiler [Item String]
loadSortedMatchingCompiledNotes = do
        allNotesList   <- loadAll ("notes/**/*.md" .&&. hasVersion "pandoc")
        classNotesList <- filterByMDMatch "course_title" allNotesList
        chronological classNotesList

--compiles the front page for the course given by the salient resource
courseCompiler = do 
        notes    <- loadSortedMatchingCompiledNotes
        firstUrl <- if Prelude.null notes then return "" else itemUrl (head notes)
        let courseCtx = 
                interpolationField "sideBar" "components/courseInfoSide.html"                         `mappend`
                (if Prelude.null notes then mempty else constField "first_note" firstUrl)             `mappend`
                courseContext
        getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" courseCtx
            >>= applyAsTemplate courseCtx
            >>= relativizeUrls

--compiles the syllabus page for the syllabus corresponding to the salient
--resource
syllabusCompiler = do 
        notes       <- loadSortedMatchingCompiledNotes
        firstUrl    <- if Prelude.null notes then return "" else itemUrl (head notes)
        resourceMD  <- getResourceMD
        syllabi     <- loadAll ("syllabi/*" .&&. hasNoVersion)
        matches     <- filterByMDMatch "course_title" syllabi
        let theSyllabus = head matches
        let syllabusCtx = 
                interpolationField "sideBar" "components/syllabusSide.html"                 `mappend`
                (if Prelude.null notes then mempty else constField "first_note" firstUrl)   `mappend`
                (tocFieldUsing theSyllabus)                                                 `mappend`
                courseContext
        return theSyllabus
            >>= makeItem . itemBody 
            >>= loadAndApplyTemplate "templates/default.html" syllabusCtx 
            >>= applyAsTemplate syllabusCtx
            >>= relativizeUrls
            --
--interleaves the toc and pandoc versions of a note, and uses those to
--populate a template for a note page
noteCompiler = do
        underlying     <- getUnderlying
        notes          <- loadSortedMatchingCompiledNotes
        firstUrl       <- itemUrl (head notes)
        let splitNotes = span (\x -> itemIdentifier x /= setVersion (Just "pandoc") underlying) notes
            noteCtx = 
                interpolationField "sideBar" "components/noteSide.html"                     `mappend`
                listField "priorNotes"  defaultContext (return $ fst splitNotes)            `mappend`
                listField "theNote"     defaultContext (return $ [head (snd splitNotes)])   `mappend`
                listField "postNotes"   defaultContext (return $ tail $ snd splitNotes)     `mappend`
                tocField                                                                    `mappend`
                constField "first_note" firstUrl                                            `mappend`
                courseContext
        (load . setVersion (Just "pandoc") $ underlying)
            >>= makeItem . itemBody
            >>= loadAndApplyTemplate "templates/default.html" noteCtx
            >>= applyAsTemplate noteCtx
            >>= relativizeUrls

--compiles the front page
frontPageCompiler = do
        courses       <- recentItemFilter 11000000 =<< recentFirst =<< loadAll ("courses/*" .&&. hasNoVersion)
        papers        <- recentFirst =<< loadAll "papers/*"
        presentations <- recentFirst =<< loadAll "presentations/*"
        let indexCtx =
                listField "courses"                   defaultContext (return courses)       `mappend`
                listField "papers"                    defaultContext (return papers)        `mappend`
                listField "presentations"             defaultContext (return presentations) `mappend`
                interpolationField "js"      "components/frontscripts.html"                 `mappend`
                interpolationField "sideBar" "components/contactSide.html"                  `mappend`
                interpolationField "navBar"  "components/frontNav.html"                     `mappend`
                defaultContext
        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

--filters out items whose publication date is more than @diff@ seconds from
--the present.
recentItemFilter :: NominalDiffTime -> [Item String] -> Compiler [Item String]
recentItemFilter diff items = do 
        currentTime <- unsafeCompiler getCurrentTime
        let utcOf x = getItemUTC defaultTimeLocale (itemIdentifier x)
        let isRecent x = (\y -> diffUTCTime currentTime y < diff ) <$> utcOf x
        filterM isRecent items
        

--gets the metadata of the salient resource
getResourceMD :: Compiler Metadata
getResourceMD = getUnderlying >>= getMetadata

--------------------------------------------------------------------------------

--this interpolates the body of the resource that the identifier points to.
--The resource must already have been compiled for it to be caught by an
--identifier. (perhaps the compilation could be wrapped into the function at some point)
interpolationField :: String -> Identifier -> Context a
interpolationField keyword interpolant = field keyword (\_ -> loadBody interpolant)

tocField :: Context a
tocField = field "toc" $ \item ->
            loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})

tocFieldUsing :: Item a -> Context b
tocFieldUsing itemUsed = field "toc" $ \item ->
            loadBody ((itemIdentifier itemUsed) { identifierVersion = Just "toc"})

mergeCases :: Show a => Either a String -> String
mergeCases (Left e) = show e
mergeCases (Right s) = s 

courseContext :: Context String
courseContext = interpolationField "navBar" "components/courseNav.html" `mappend`
                defaultContext 

echoMD :: MonadMetadata m => String -> Item a -> m (Maybe String)
echoMD field item = do
        metadata <- getMetadata (itemIdentifier item)
        return $ M.lookup field metadata

matchUnderlyingMD :: String -> Item a -> Compiler Bool
matchUnderlyingMD field item = do
        itemMDfield <- echoMD field item
        resourceMD <- getResourceMD
        let resourceMDfield = M.lookup field resourceMD
        return (itemMDfield == resourceMDfield)

filterByMDMatch :: String -> [Item a] -> Compiler [Item a]
filterByMDMatch fld = filterM (matchUnderlyingMD fld)

itemUrl :: Item a -> Compiler String
itemUrl = fmap (maybe "" toUrl) . getRoute . itemIdentifier

noteRoute :: Metadata -> Routes
noteRoute m = constRoute $ 
    ctCatch (M.lookup "course_title" m) ++ "/" ++ ntCatch(M.lookup "note_title" m) ++ ".html"
    where ctCatch ( Just s )  = cleanName s
          ctCatch ( Nothing ) = "NoTitle"
          ntCatch ( Just s  ) = cleanName s
          ntCatch ( Nothing ) = "Untited_Note"

-- returns a route based on the course title in the metadata, plus the page
-- title fed into the function, for use with 
-- metadataRoute:: (Metadata -> Routes) -> Routes
courseRoute :: String -> Metadata -> Routes
courseRoute title m = constRoute $ 
    ctCatch (M.lookup "course_title" m) ++ "_"
    ++ ctCatch (M.lookup "course_section" m) ++ "_" 
    ++ ctCatch (M.lookup "semester" m) ++ "/" ++ title
    where ctCatch ( Just s )  =  cleanName s
          ctCatch ( Nothing ) = ""

--removes spaces, for use of a title as a url.
cleanName :: String -> String
cleanName = Prelude.map (\x -> if x == ' ' then '_' else x)
