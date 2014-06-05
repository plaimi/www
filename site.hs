{-
- Copyright (C) 2013, 2014 plaimi <www@plaimi.net>
-
- Copying and distribution of this file, with or without modification,
- are permitted in any medium without royalty provided the copyright
- notice and this notice are preserved.  This file is offered as-is,
- without any warranty.
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))

import Hakyll.Core.Configuration (Configuration, defaultConfiguration
                                 ,deployCommand)
import Hakyll.Core.Compiler (getResourceBody, loadAll, makeItem)
import Hakyll.Core.File (copyFileCompiler)
import Hakyll.Core.Identifier.Pattern (fromList)
import Hakyll.Core.Routes (idRoute, setExtension)
import Hakyll.Core.Rules (compile, create, match, route)
import Hakyll.Main (hakyllWith)
import Hakyll.Web.CompressCss (compressCssCompiler)
import Hakyll.Web.Html.RelativizeUrls (relativizeUrls)
import Hakyll.Web.Pandoc (pandocCompiler)
import Hakyll.Web.Template (applyAsTemplate, loadAndApplyTemplate
                           ,templateCompiler)
import Hakyll.Web.Template.Context (constField, dateField, defaultContext
                                   ,listField)
import Hakyll.Web.Template.List (recentFirst)

import Config (synchCommand, synchTarget)

main ::  IO ()
main = hakyllWith configuration $ do
  match "templates/*" $ compile templateCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList ["about.markdown"
                  ,"contact.markdown"
                  ,"swag.markdown"
                  ,"~alexander/haskell.markdown"
                  ,"~alexander/index.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route   idRoute
    compile $ do
      games <- loadAll "games/*"
      works <- loadAll "works/*"
      other <- loadAll "other/*"
      news  <- recentFirst =<< loadAll "news/*"
      let newsCtx  = dateField "date" "%B %e, %Y" <> defaultContext
      let indexCtx =
            listField "news"  newsCtx        (return news)  <>
            listField "games" defaultContext (return games) <>
            listField "works" defaultContext (return works) <>
            listField "other" defaultContext (return other) <>
            defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "mailing.html" $ do
    route   idRoute
    compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
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
                     constField "title" "other"                      <>
                     defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/other-list.html" otherCtx
        >>= loadAndApplyTemplate "templates/default.html"    otherCtx
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

configuration ::  Configuration
configuration =
  defaultConfiguration {
                       deployCommand = unwords [synchCommand, synchTarget]
                       }
