--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import qualified Data.Text as T
import qualified GHC.IO.Encoding as E
import Hakyll
import Lucid
import Lucid.Base
import Lucid.Html5
import Protolude
import Prelude (String)

-------------------------------------------------------------------------------
main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "images/hand-writing/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match (fromList ["about.markdown", "contact.markdown"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx >>=
        loadAndApplyTemplate "templates/default.html" postCtx >>=
        applyFilter youtubeFilter >>=
        relativizeUrls
    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Archives" `mappend`
              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
          loadAndApplyTemplate "templates/default.html" archiveCtx >>=
          relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Home" `mappend`
              defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

youtubeVideo vidId =
  renderText $ do
    div_ [class_ "video-wrapper"] $ do
      div_ [class_ "video-container"] $ do
        iframe_
          [ src_ $ "https://www.youtube.com/embed/" <> vidId
          , makeAttribute "frameborder" "0"
          ]
          mempty

{- From: http://www.jonashietala.se/blog/2014/09/01/embedding_youtube_videos_with_hakyll/ -}
-- Find and replace bare youtube links separated by <p></p>.
youtubeFilter :: String -> String
youtubeFilter = replaceAll regex (result . toS . extractID)
  where
    regex = "<p>https?://www\\.youtube\\.com/watch\\?v=([A-Za-z0-9_-]+)</p>"
    result = toS . youtubeVideo
    extractID :: String -> String
    extractID =
      reverse .
      drop 4 .
      reverse . drop (length ("<p>https://www.youtube.com/watch?v=" :: String))

applyFilter ::
     (Monad m, Functor f) => (String -> String) -> f String -> m (f String)
applyFilter transformator str = return $ (fmap $ transformator) str
