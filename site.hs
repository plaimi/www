{-# LANGUAGE OverloadedStrings #-}

{-
- Copyright (C) 2013-2015 plaimi <www@plaimi.net>
-
- Copying and distribution of this file, with or without modification,
- are permitted in any medium without royalty provided the copyright
- notice and this notice are preserved.  This file is offered as-is,
- without any warranty.
-}

import Control.Monad
  (
  filterM,
  )
import Data.Char
  (
  toUpper,
  )
import Data.Monoid
  (
  (<>),
  )
import Hakyll.Core.Configuration
  (
  Configuration,
  defaultConfiguration,
  deployCommand,
  )
import Hakyll.Core.Compiler
  (
  getResourceBody,
  loadAll,
  makeItem,
  )
import Hakyll.Core.File
  (
  copyFileCompiler,
  )
import Hakyll.Core.Identifier
  (
  fromFilePath,
  )
import Hakyll.Core.Identifier.Pattern
  (
  Pattern,
  fromList,
  )
import Hakyll.Core.Item
  (
  Item,
  itemIdentifier,
  )
import Hakyll.Core.Metadata
  (
  MonadMetadata,
  getMetadataField,
  )
import Hakyll.Core.Routes
  (
  idRoute,
  setExtension,
  )
import Hakyll.Core.Rules
  (
  Rules,
  compile,
  create,
  match,
  route,
  )
import Hakyll.Main
  (
  hakyllWith,
  )
import Hakyll.Web.CompressCss
  (
  compressCssCompiler,
  )
import Hakyll.Web.Html.RelativizeUrls
  (
  relativizeUrls,
  )
import Hakyll.Web.Pandoc
  (
  pandocCompiler,
  )
import Hakyll.Web.Template
  (
  applyAsTemplate,
  loadAndApplyTemplate,
  templateCompiler,
  )
import Hakyll.Web.Template.Context
  (
  constField,
  dateField,
  defaultContext,
  listField,
  )
import Hakyll.Web.Template.List
  (
  recentFirst,
  )

import Config
  (
  synchCommand,
  synchTarget,
  )

main ::  IO ()
main = hakyllWith configuration $ do

  -- Templates
  match "templates/*" $ compile templateCompiler

  -- CSS
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  -- Pure file copy.
  match (fromList ["~alexander/contact.txt"
                  ,"~olle/contact.txt"
                  ,"~emil/contact.txt"
                  ]) copyFiles

  -- Pure file copy with globs.
  match "papers/*.pdf"       copyFiles
  match "images/*"           copyFiles
  match "presentations/*pdf" copyFiles

  match (fromList ["about.markdown"
                  ,"contact.markdown"
                  ,"swag.markdown"
                  ,"~alexander/haskell.markdown"
                  ,"~alexander/index.markdown"
                  ,"~olle/index.markdown"
                  ,"~emil/index.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- Markdown to HTML with template applied.
  match "projects/*.markdown"      $ mdToHtml "project"
  match "news/*.markdown"          $ mdToHtml "news"
  match "works/*.markdown"         $ mdToHtml "work"
  match "games/*.markdown"         $ mdToHtml "game"
  match "papers/*.markdown"        $ mdToHtml "paper"
  match "presentations/*.markdown" $ mdToHtml "presentation"
  match "other/*.markdown"         $ mdToHtml "other"

  -- Markdown to more category HTML sites. Category sites list all entries in
  -- that specific category.
  create ["works.html"] $ toCategoryHtml "works/*.markdown" "works" "work"
  create ["games.html"] $ toCategoryHtml "games/*.markdown" "games" "game"
  create ["other.html"] $ toCategoryHtml "other/*.markdown" "other" "other"

  -- Markdown to archive HTML sites. Archives are like category sites but
  -- date-sorted.
  create ["news.html"]          $ toArchiveHtml "news/*.markdown"
                                                "news" "news"
  create ["papers.html"]        $ toArchiveHtml "papers/*.markdown"
                                                "papers" "paper"
  create ["presentations.html"] $ toArchiveHtml "presentations/*.markdown"
                                                "presentations" "presentation"

  -- index.html. It's messy. Sorry.
  match "index.html" $ do
    route   idRoute
    compile $ do
      news        <- recentFirst =<< loadAll "news/*.markdown"
      games       <-                 loadAll "games/*.markdown"
      works       <-                 loadAll "works/*.markdown"
      papers      <- recentFirst =<< loadAll "papers/*.markdown"
      pres        <- recentFirst =<< loadAll "presentations/*.markdown"
      other       <-                 loadAll "other/*.markdown"
      let newsCtx  = dateField "date" "%B %e, %Y"
                  <> defaultContext
      let gameCtx  = listField "games"         defaultContext (return games)
                  <> defaultContext
      let workCtx  = listField "works"         defaultContext (return works)
                  <> defaultContext
      let paperCtx = dateField "date" "%B %e, %Y"
                  <> listField "papers"        defaultContext (return papers)
                  <> defaultContext
      let presCtx  = dateField "date" "%B %e, %Y"
                  <> listField "presentations" defaultContext (return pres)
                  <> defaultContext
      let otherCtx = listField "other"         defaultContext (return other)
                  <> defaultContext
      let indexCtx = listField "news"          newsCtx        (return news)
                  <> listField "works"         workCtx        (return works)
                  <> listField "games"         gameCtx        (return games)
                  <> listField "papers"        paperCtx       (return papers)
                  <> listField "presentations" presCtx        (return pres)
                  <> listField "other"         otherCtx       (return other)
                  <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "mailing.html" $ do
    route   idRoute
    compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- projects.html: list of all projects.
  create ["projects.html"] $ do
    route   idRoute
    compile $ do
      games  <-                 loadAll "games/*.markdown"
      works  <-                 loadAll "works/*.markdown"
      papers <- recentFirst =<< loadAll "papers/*.markdown"
      pres   <- recentFirst =<< loadAll "presentations/*.markdown"
      other  <-                 loadAll "other/*.markdown"
      let ctx = constField "title" "Projects"
             <> listField "games"         defaultContext (return games)
             <> listField "works"         defaultContext (return works)
             <> listField "papers"        defaultContext (return papers)
             <> listField "presentations" defaultContext (return pres)
             <> listField "other"         defaultContext (return other)
             <> defaultContext
      makeItem "" >>= loadAndApplyTemplate "templates/project-list.html" ctx
                  >>= loadAndApplyTemplate "templates/default.html"      ctx
                  >>= relativizeUrls

toArchiveHtml :: Pattern -> String -> String -> Rules ()
toArchiveHtml p f t = do
  route idRoute
  compile $ do
    u <- recentFirst =<< loadAll p
    let ctx  = dateField "date" "%B %e, %Y" <> defaultContext
    let actx = listField f ctx (return u)
            <> constField "title" (toUpper (head f) : tail f)
            <> defaultContext
    makeItem "" >>= loadAndApplyTemplate (fromFilePath $ "templates/"
                                                      ++ t
                                                      ++ "-list.html") actx
                >>= loadAndApplyTemplate "templates/default.html"      actx
                >>= relativizeUrls

toCategoryHtml :: Pattern -> String -> String -> Rules ()
toCategoryHtml p f t = do
  route idRoute
  compile $ do
    u <- loadAll p
    let ctx = listField f defaultContext (return u)
           <> constField "title" (toUpper (head f) : tail f)
           <> defaultContext
    makeItem "" >>= loadAndApplyTemplate (fromFilePath $ "templates/"
                                                      ++ t
                                                      ++ "-list.html") ctx
                >>= loadAndApplyTemplate "templates/default.html"      ctx
                >>= relativizeUrls

mdToHtml :: String -> Rules ()
mdToHtml t = do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate (fromFilePath $ "templates/" ++ t ++ ".html") defaultContext
    >>= loadAndApplyTemplate "templates/default.html"       defaultContext
    >>= relativizeUrls

copyFiles :: Rules ()
copyFiles = route idRoute >> compile copyFileCompiler

filterType :: (Functor f, MonadMetadata f) => String -> [Item a] -> f [Item a]
filterType t = filterM ((\_ -> fmap (maybe False (== t)) .
            flip getMetadataField "type" . itemIdentifier) t)

configuration ::  Configuration
configuration =
  defaultConfiguration {
                       deployCommand = unwords [synchCommand, synchTarget]
                       }
