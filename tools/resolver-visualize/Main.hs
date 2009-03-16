#!/usr/bin/env runhaskell

module Main where

import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
import Data.ByteString.Char8 as ByteString(ByteString, empty, readFile, unpack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.SourceView
import System.Environment(getArgs)
import System.Glib.MainLoop
import Data.IORef

xmlFilename = "resolver-visualize.glade"

-- | Shared context for the visualizer.
data VisualizeContext =
    VisualizeContext { treeView :: TreeView,
                       chronologicalView :: TreeView,
                       logView :: SourceView,
                       logText :: IORef ByteString }

-- | State monad for the visualizer.
type VisM = ReaderT VisualizeContext IO

-- Accessors for state.
runVis :: (VisM a) -> VisualizeContext -> IO a
runVis = runReaderT

getTreeView :: VisM TreeView
getTreeView = do ctx <- ask; return $ treeView ctx

getChronologicalView :: VisM TreeView
getChronologicalView = do ctx <- ask; return $ treeView ctx

getLogView :: VisM SourceView
getLogView = do ctx <- ask; return $ logView ctx


getLogText :: VisM ByteString
getLogText = do ctx <- ask; (liftIO $ readIORef $ logText ctx) >>= return

setLogText :: ByteString -> VisM ()
setLogText v = do ctx <- ask; liftIO $ writeIORef (logText ctx) v



loadLog :: FilePath -> VisM ()
loadLog fn = do rawLogView <- getLogView
                contents <- liftIO $ ByteString.readFile fn
                rawLogViewBuffer <- liftIO $ textViewGetBuffer rawLogView
                -- Now write back the new contents:
                setLogText contents
                liftIO $ textBufferSetText rawLogViewBuffer (unpack contents)



loadXml :: IO GladeXML
loadXml = do x <- xmlNew xmlFilename
             case x of
               Nothing -> error ("Can't load " ++ (show xmlFilename))
               Just xml -> return xml

setupMainWindow :: GladeXML -> IO SourceView
setupMainWindow xml =
    do rightPane <- xmlGetWidget xml castToHPaned "hpaned2"
       sourceView <- sourceViewNew
       sourceViewSetShowLineNumbers sourceView True
       textViewSetEditable sourceView False
       panedAdd2 rightPane sourceView
       return sourceView

newCtx :: GladeXML -> IO VisualizeContext
newCtx xml = do sourceView <- setupMainWindow xml
                tv <- xmlGetWidget xml castToTreeView "tree_view_search_tree"
                chrontv <- xmlGetWidget xml castToTreeView "tree_view_search_history"
                text <- newIORef (ByteString.empty)
                return VisualizeContext { treeView = tv,
                                          chronologicalView = chrontv,
                                          logView = sourceView,
                                          logText = text }

main :: IO ()
main = do -- Gtk2Hs whines loudly if it gets loaded into a threaded
          -- runtime, but runhaskell always loads a threaded runtime,
          -- so we have to call this to be a script:
          unsafeInitGUIForThreadedRTS
          xml <- loadXml
          mainWindow <- xmlGetWidget xml castToWindow "main_window"
          -- Setup the new top-level context.
          ctx <- newCtx xml
          mainLoop <- mainLoopNew Nothing False
          on mainWindow deleteEvent (liftIO (mainLoopQuit mainLoop) >> return True)
          widgetShowAll mainWindow
          args <- getArgs
          runVis (loadLog (head args)) ctx
          mainLoopRun mainLoop

