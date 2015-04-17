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
import Hakyll.Core.Identifier.Pattern
  (
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
  match "templates/*" $ compile templateCompiler

  match "images/*" copyFiles

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList ["~alexander/contact.txt"
                  ,"~olle/contact.txt"
                  ]) copyFiles

  match (fromList ["about.markdown"
                  ,"contact.markdown"
                  ,"swag.markdown"
                  ,"~alexander/haskell.markdown"
                  ,"~alexander/index.markdown"
                  ,"~olle/index.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route   idRoute
    compile $ do
      let projects = loadAll "projects/*"
      news        <- recentFirst               =<< loadAll "news/*"
      games       <- filterType "game"         =<< projects
      works       <- filterType "work"         =<< projects
      papers      <- recentFirst               =<< filterType "paper"
                                               =<< projects
      pres        <- recentFirst               =<< filterType "presentation"
                                               =<< projects
      other       <- filterType "other"        =<< projects
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

  match "projects/*.markdown" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/project.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["projects.html"] $ do
    route   idRoute
    compile $ do
      let projects = loadAll "projects/*"
      games       <- filterType "game"         =<< projects
      works       <- filterType "work"         =<< projects
      papers      <- filterType "paper"        =<< projects
      pres        <- filterType "presentation" =<< projects
      other       <- filterType "other"        =<< projects
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

  match "news/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/news.html"    defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["news.html"] $ do
    route   idRoute
    compile $ do
      news <- recentFirst =<< loadAll "news/*"
      let newsCtx    = dateField "date" "%B %e, %Y" <> defaultContext
      let archiveCtx = listField "news"  newsCtx (return news) <>
                       constField "title" "News"               <>
                       defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/news-list.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html"   archiveCtx
        >>= relativizeUrls

  match "games/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/game.html"    defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["games.html"] $ do
    route   idRoute
    compile $ do
      games <- loadAll "games/*"
      let gamesCtx = listField "games" defaultContext (return games) <>
                     constField "title" "Games"                      <>
                     defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/game-list.html" gamesCtx
        >>= loadAndApplyTemplate "templates/default.html"   gamesCtx
        >>= relativizeUrls

  match "works/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/work.html"    defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["works.html"] $ do
    route   idRoute
    compile $ do
      works <- loadAll "works/*"
      let worksCtx = listField "works" defaultContext (return works) <>
                     constField "title" "Works"                      <>
                     defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/work-list.html" worksCtx
        >>= loadAndApplyTemplate "templates/default.html"   worksCtx
        >>= relativizeUrls

  match "other/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/other.html"   defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["other.html"] $ do
    route   idRoute
    compile $ do
      other <- loadAll "other/*"
      let otherCtx = listField "other" defaultContext (return other) <>
                     constField "title" "Other"                      <>
                     defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/other-list.html" otherCtx
        >>= loadAndApplyTemplate "templates/default.html"    otherCtx
        >>= relativizeUrls

  match "papers/*.markdown" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/paper.html"  defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "papers/*.pdf" copyFiles

  create ["papers.html"] $ do
    route   idRoute
    compile $ do
      papers <- recentFirst =<< loadAll "papers/*.markdown"
      let papersCtx  = dateField "date" "%B %e, %Y" <> defaultContext
      let archiveCtx = listField "papers"  papersCtx (return papers)
                    <> constField "title" "Papers"
                    <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/paper-list.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html"    archiveCtx
        >>= relativizeUrls

  match "presentations/*.markdown" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/presentation.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html"      defaultContext
        >>= relativizeUrls

  match "presentations/*pdf" copyFiles

  create ["presentations.html"] $ do
    route   idRoute
    compile $ do
      pres <- recentFirst =<< loadAll "presentations/*.markdown"
      let presCtx  = dateField "date" "%B %e, %Y" <> defaultContext
      let archiveCtx = listField "presentations"  presCtx (return pres)
                    <> constField "title" "Presentations"
                    <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/presentation-list.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html"           archiveCtx
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
