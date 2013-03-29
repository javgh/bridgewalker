{-# LANGUAGE OverloadedStrings #-}

-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module SnapSite
  ( snapApp
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS

import CommonTypes
import Config
import WebsocketBackend

-- | Ping interval for /deadend handler
websocketPingInterval :: Int
websocketPingInterval = 30

-- | The application's routes.
routes :: BridgewalkerHandles -> [(ByteString, Handler SnapApp SnapApp ())]
routes bwHandles =
    [ ("", serveDirectory "static")
    , ("/deadend", liftSnap (WS.runWebSocketsSnap webSocketApp))
    , ("/backend", liftSnap (WS.runWebSocketsSnap (websocketBackend bwHandles)))
    ]

--fortyTwoSplice :: SnapletHeist SnapApp SnapApp [X.Node]
--fortyTwoSplice = return [X.TextNode $ "42"]

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

prepareWSReply :: ToJSON a => a -> T.Text
prepareWSReply = T.decodeUtf8 . toStrict . encode

webSocketApp :: WS.Request -> WS.WebSockets WS.Hybi10 ()
webSocketApp rq = do
    let notUnderstood = object [ "reply" .= ("not_understood" :: T.Text)
                               , "info" .= ("not implemented yet" :: T.Text)
                               ]
    WS.acceptRequest rq
    WS.spawnPingThread websocketPingInterval
    WS.sendTextData . prepareWSReply $ notUnderstood

------------------------------------------------------------------------------
-- | The application initializer.
snapApp :: BridgewalkerHandles -> SnapletInit SnapApp SnapApp
snapApp bwHandles = makeSnaplet "bridgewalker" "Bridgewalker" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addRoutes $ routes bwHandles
    --addSplices [ ("fortytwo", fortyTwoSplice) ]
    return $ SnapApp h
