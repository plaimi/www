{-
- Copyright (C) 2013 Alexander Berntsen <alexander@plaimi.net>
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
                  ,"~alexander/index.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route   idRoute
    compile $ do
      games <- loadAll "games/*"
      let indexCtx = listField "games" defaultContext (return games) <>
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
  where synchCommand = "rsync -ave 'ssh -p [port] _site"
        synchTarget  = "[user]@[host]:/[path]/"
