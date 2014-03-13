{-# LANGUAGE OverloadedStrings #-}

-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module SnapSite
  ( snapApp
  ) where

import Data.ByteString (ByteString)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe

import CommonTypes

-- | The application's routes.
routes :: [(ByteString, Handler SnapApp SnapApp ())]
routes =
    [ ("", serveDirectory "static")
    ]

------------------------------------------------------------------------------
-- | The application initializer.
snapApp :: SnapletInit SnapApp SnapApp
snapApp = makeSnaplet "bridgewalker" "Bridgewalker" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addRoutes routes
    return $ SnapApp h
