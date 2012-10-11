{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM_)
import Control.Arrow (arr, (>>>), (***), second, (&&&))
import Data.Monoid (mempty, mconcat)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze ((!), toValue)
import System.FilePath (joinPath, splitDirectories, takeDirectory)
import Data.List (isInfixOf)
import Text.HTML.TagSoup (Tag (..), renderTags, parseTags)
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Set as S

import Hakyll

cssPath   = parseGlob "css/*"
imgPath   = parseGlob "img/*"
jsPath    = parseGlob "js/*"
fontPath  = parseGlob "font/*"
postsPath = parseGlob "posts/*"
tagsPath  = parseGlob "posts/tag/*"
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
                      compile $ postCompiler

    -- Build up index/posts pages.
    createAndRenderIndex "index.html" "Home"  mostRecent  "templates/postitem.html"  "templates/index.html"
    createAndRenderIndex "posts.html" "Posts" recentFirst "templates/postshort.html" "templates/posts.html"

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

createAndRenderTags c path readF = do
    create c        $ requireAll "posts/*" (\_ ps -> readF ps :: Tags String)
    match path      $ do
                      route       $  setExtension ".html"
                      metaCompile $  require_ c
                                 >>> arr tagsMap
                                 >>> arr (map (\(t, p) -> (fromCapture path t, makeTagList t p)))

createAndRenderIndex path title whichPosts postTmplt tmplt = do
        match path     $  route idRoute
        create pathPat $  constA mempty
                      >>> arr (setField "pagetitle" title)
                      >>> setFieldPageList whichPosts postTmplt "posts" "posts/*"
                      >>> applyTemplateCompiler tmplt
                      >>> applyTemplateCompiler "templates/default.html"
                      >>> relativizeUrlsCompiler
    where pathPat = fromCapture path ""


mostRecent = take 1 . recentFirst

-- | Turns body of the page into the teaser: anything up to the
-- <!--MORE--> mark is the teaser, except for text between the
-- <!--NOTEASERBEGIN--> and <!--NOTEASEREND--> marks (useful for
-- keeping images out of teasers).
--
renderTeaser field =  arr (copyBodyToField field)
               >>> arr (changeField field extractTeaser)
      where
        extractTeaser  = unlines . (noTeaser . extractTeaser') . lines
        extractTeaser' = takeWhile (/= "<!--MORE-->")

        noTeaser [] = []
        noTeaser ("<!--NOTEASERBEGIN-->" : xs) = 
          drop 1 $ dropWhile (/= "<!--NOTEASEREND-->") xs
        noTeaser (x : xs) = x : (noTeaser xs)
