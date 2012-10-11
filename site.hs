{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (arr, (>>>), Arrow)
import Data.Monoid (mempty)

import Hakyll

cssPath ::  Pattern a
cssPath   = parseGlob "css/*"

imgPath ::  Pattern a
imgPath   = parseGlob "img/*"

jsPath ::  Pattern a
jsPath    = parseGlob "js/*"

fontPath ::  Pattern a
fontPath  = parseGlob "font/*"

postsPath ::  Pattern a
postsPath = parseGlob "posts/*"

tagsPath ::  Pattern a
tagsPath  = parseGlob "posts/tag/*"

catsPath ::  Pattern a
catsPath  = parseGlob "posts/category/*"

main :: IO ()
main = hakyll $ do
    -- Copy CSS, Images, JavaScript and Fonts
    match cssPath  copyCompiler
    match imgPath  copyCompiler
    match jsPath   copyCompiler
    match fontPath copyCompiler
    match "CNAME"  copyCompiler

    -- Route and Render posts
    match postsPath $ do
                      route   $ setExtension ".html"
                      compile postCompiler

    -- Build up index/posts pages.
    createAndRenderIndex "index.html" "Home"  mostRecent
        "templates/postitem.html"  "templates/index.html"
    createAndRenderIndex "posts.html" "Posts" recentFirst
        "templates/postshort.html" "templates/posts.html"

    -- Build up tags/categories and their pages.
    createAndRenderTags "tags"       tagsPath readTags
    createAndRenderTags "categories" catsPath readCategory

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- End
    return ()

-- | Main post compiler
postCompiler :: Compiler Resource (Page String)
postCompiler =  pageCompiler
            >>> arr (copyBodyToField "content")
            >>> arr (renderDateField "date" "%A, %B %e, %Y" "")
            >>> arr (renderDateField "xmldate" "%F" "")
            >>> renderCategoryField "postcategory" (fromCapture catsPath)
            >>> renderTagsField "posttags" (fromCapture tagsPath)
            >>> renderTeaser "teaser"
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

copyCompiler ::  RulesM (Pattern CopyFile)
copyCompiler = do
               route idRoute
               compile copyFileCompiler

makeTagList :: String -> [Page String] -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
        >>> pageListCompiler recentFirst "templates/postshort.html"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "pagetitle" ("Posts from " ++ tag))
        >>> arr (setField "taglabel" tag)
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

createAndRenderTags :: Identifier (Tags String)
                    -> Pattern (Page String)
                    -> ([Page String] -> Tags String)
                    -> RulesM ()
createAndRenderTags c path readF = do
        create c   $ requireAll "posts/*" (\ _ ps -> readF ps :: Tags String)
        match path $ do
                     route       $  setExtension ".html"
                     metaCompile $  require_ c
                                >>> arr tagsMap
                                >>> arr (map compileTag)
    where compileTag (t, p) = (fromCapture path t, makeTagList t p)

createAndRenderIndex :: Pattern (Page String)
                     -> String
                     -> ([Page String]
                     -> [Page String])
                     -> Identifier Template
                     -> Identifier Template
                     -> RulesM (Identifier (Page String))
createAndRenderIndex path title whichPosts postTmplt tmplt = do
        match path     $  route idRoute
        create pathPat $  constA mempty
                      >>> arr (setField "pagetitle" title)
                      >>> setFieldPageList
                            whichPosts postTmplt "posts" "posts/*"
                      >>> applyTemplateCompiler tmplt
                      >>> applyTemplateCompiler "templates/default.html"
                      >>> relativizeUrlsCompiler
    where pathPat = fromCapture path ""


mostRecent ::  [Page a] -> [Page a]
mostRecent = take 1 . recentFirst

{- | Turns body of the page into the teaser: anything up to the
 -   <!--MORE--> mark is the teaser, except for text between the
 -   <!--NOTEASERBEGIN--> and <!--NOTEASEREND--> marks (useful for
 -   keeping images out of teasers). -}
renderTeaser :: Control.Arrow.Arrow cat
             => String
             -> cat (Page String) (Page String)
renderTeaser field =  arr (copyBodyToField field)
               >>> arr (changeField field extractTeaser)
      where
        extractTeaser  = unlines . (noTeaser . extractTeaser') . lines
        extractTeaser' = takeWhile (/= "<!--MORE-->")

        noTeaser [] = []
        noTeaser ("<!--NOTEASERBEGIN-->" : xs) =
          drop 1 $ dropWhile (/= "<!--NOTEASEREND-->") xs
        noTeaser (x : xs) = x : noTeaser xs
