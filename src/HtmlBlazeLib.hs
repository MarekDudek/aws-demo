{-# LANGUAGE OverloadedStrings, DataKinds #-}

module HtmlBlazeLib where


import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Network.Wai.Handler.Warp


type API = Get '[HTML] Homepage
type Homepage = H.Html

server :: Server API
server = return myHome

myHome :: Homepage
myHome = H.docTypeHtml $ do
  H.head $ do
    H.title "Live to serve"
  H.body $ do
    H.h1 "Templates"
    H.p "This will be type-checked, rendered and served"


htmlAPI :: Proxy API
htmlAPI = Proxy

htmlApp :: Application
htmlApp = serve htmlAPI server

runMyHome :: IO ()
runMyHome = run 8080 htmlApp
