-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Gtk
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Gtk(
    renderableToWindow,
    toWindow,
    createRenderableWindow,
    updateCanvas
    ) where

import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as GE
import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.State(EC, execEC)

import Data.List (isPrefixOf)
import Data.IORef
import Data.Default.Class

import Control.Monad(when)
import Control.Monad.Trans (liftIO)
import System.IO.Unsafe(unsafePerformIO)

--- do action m for any keypress (except modified keys)
anyKey :: IO () -> G.EventM G.EKey Bool
anyKey m = G.tryEvent $ do
    -- Unfortunately, gtk2hs doesn't currently expose
    -- EventKey.is_modifier, and eventModifier doesn't include the
    -- current key if it is a modifier, so resort to seeing if the key
    -- has no character representation, which is roughly the same
    -- except that it gives a false positive for e.g. arrow keys
    (Just _) <- G.keyToChar `fmap` G.eventKeyVal
    liftIO m

-- Yuck. But we really want the convenience function
-- renderableToWindow as to be callable without requiring
-- initGUI to be called first. But newer versions of
-- gtk insist that initGUI is only called once
guiInitVar :: IORef Bool
{-# NOINLINE guiInitVar #-}
guiInitVar = unsafePerformIO (newIORef False)

initGuiOnce :: IO ()
initGuiOnce = do
    v <- readIORef guiInitVar
    when (not v) $ do
        -- G.initGUI
        G.unsafeInitGUIForThreadedRTS
        writeIORef guiInitVar True

-- | Display a renderable in a gtk window.
--
-- Note that this is a convenience function that initialises GTK on
-- it's first call, but not subsequent calls. Hence it's 
-- unlikely to be compatible with other code using gtk. In 
-- that case use createRenderableWindow.
renderableToWindow :: Renderable a -> Int -> Int -> IO ()
renderableToWindow chart windowWidth windowHeight = do
    initGuiOnce
    window <- createRenderableWindow chart windowWidth windowHeight
    -- press any key to exit the loop
    G.on window G.keyPressEvent $ anyKey (G.widgetDestroy window)
    G.on window G.destroyEvent $ liftIO $ G.mainQuit >> return True
    G.widgetShowAll window
    G.mainGUI

-- | Generate a new GTK window from the state content of
-- an EC computation. The state may have any type that is
-- an instance of `ToRenderable`
toWindow :: (Default r, ToRenderable r) =>Int -> Int -> EC r () -> IO ()
toWindow windowWidth windowHeight ec = renderableToWindow r windowWidth windowHeight where
                       r = toRenderable (execEC ec)

-- | Create a new GTK window displaying a renderable.
createRenderableWindow :: Renderable a -> Int -> Int -> IO G.Window
createRenderableWindow chart windowWidth windowHeight = do
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    G.widgetSetSizeRequest window windowWidth windowHeight
    G.on canvas G.draw $ updateCanvas chart canvas
    G.set window [G.containerChild G.:= canvas]
    return window


updateCanvas :: Renderable a -> G.DrawingArea  -> C.Render ()
updateCanvas chart canvas = do
    width <- liftIO $ G.widgetGetAllocatedWidth canvas
    height <- liftIO $ G.widgetGetAllocatedHeight canvas
    let sz = (fromIntegral width,fromIntegral height)
    runBackend (defaultEnv bitmapAlignmentFns) (render chart sz)
    return ()
