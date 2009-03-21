#!/usr/bin/env runhaskell

module Main where

import Control.Monad.Reader
import qualified Control.Monad.State as State
import Control.Monad.Trans(liftIO)
import Data.ByteString.Char8 as ByteString(ByteString, empty, readFile, unpack)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.SourceView
import Numeric
import System.Environment(getArgs)
import System.Glib.MainLoop
import System.Glib.Types
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import Data.Tree
import Resolver.Log
import Resolver.Types
import Resolver.Util
import System.IO
import System.Time

xmlFilename = "resolver-visualize.glade"

-- Column numbers.
treeViewDisplayColumnText = 0

data MainLoopContext = MainLoopContext { mainLoop :: MainLoop,
                                         numMainWindows :: IORef Integer }

type MainM = ReaderT MainLoopContext IO

runMain :: (MainM a) -> MainLoopContext -> IO a
runMain = runReaderT

mainWindowClosed :: MainM ()
mainWindowClosed = do ctx <- ask
                      liftIO $ do modifyIORef (numMainWindows ctx) (+(-1))
                                  num <- readIORef (numMainWindows ctx)
                                  when (num == 0) (mainLoopQuit (mainLoop ctx))

mainWindowOpened :: MainM ()
mainWindowOpened = do ctx <- ask
                      liftIO $ modifyIORef (numMainWindows ctx) (+1)

-- | Information about a loaded log file.
data LoadedLogFile =
    LoadedLogFile { treeViewStore :: TreeStore TreeViewEntry,
                    logFile :: LogFile }

-- | Information about the columns and renderers of the tree display.
data TreeViewColumnInfo =
    TreeViewColumnInfo { treeViewTextColumn   :: TreeViewColumn,
                         treeViewTextRenderer :: CellRendererText }

-- | Information about the columns and renderers of the chronological
-- display.
data ChronologicalViewColumnInfo =
    ChronologicalViewColumnInfo

-- | Shared context for the visualizer.
data VisualizeContext =
    VisualizeContext { treeView :: TreeView,
                       treeViewColumnInfo :: TreeViewColumnInfo,
                       chronologicalView :: TreeView,
                       chronologicalViewColumnInfo :: ChronologicalViewColumnInfo,
                       logView :: SourceView,
                       mainContext :: MainLoopContext,
                       loadedFile :: IORef (Maybe LoadedLogFile) }

-- | State monad for the visualizer.
type VisM = ReaderT VisualizeContext IO

-- Accessors for state.
runVis :: (VisM a) -> VisualizeContext -> IO a
runVis = runReaderT

getTreeView :: VisM TreeView
getTreeView = do ctx <- ask; return $ treeView ctx

getTreeViewColumnInfo :: VisM TreeViewColumnInfo
getTreeViewColumnInfo = do ctx <- ask; return $ treeViewColumnInfo ctx

getChronologicalView :: VisM TreeView
getChronologicalView = do ctx <- ask; return $ treeView ctx

getChronologicalViewColumnInfo :: VisM ChronologicalViewColumnInfo
getChronologicalViewColumnInfo = do ctx <- ask; return $ chronologicalViewColumnInfo ctx

getLogView :: VisM SourceView
getLogView = do ctx <- ask; return $ logView ctx

getMainCtx :: VisM MainLoopContext
getMainCtx = do ctx <- ask; return $ mainContext ctx


getLog :: VisM (Maybe LogFile)
getLog = do ctx <- ask
            maybeInf <- liftIO $ readIORef $ loadedFile ctx
            return $ fmap logFile maybeInf

getTreeViewStore :: VisM (Maybe (TreeStore TreeViewEntry))
getTreeViewStore = do ctx <- ask
                      maybeInf <- liftIO $ readIORef $ loadedFile ctx
                      return $ fmap treeViewStore maybeInf

-- Each row in the tree display is either the root of the search, a
-- processing step (possibly with no parent!), a solution that was
-- enqueued but never visited, or a note about a problem rendering the
-- tree.
data TreeViewEntry = Root ProcessingStep
                   | Step ProcessingStep LinkChoice
                   -- | A Step node would have been produced, but the
                   -- same solution was already generated as a Step
                   -- node.  The attached Solution node could be used
                   -- to, e.g., look up the tree node associated with
                   -- the link.
                   | AlreadyGeneratedStep Solution LinkChoice
                   | NoStep Solution LinkChoice
                   | Error String -- Something went wrong.
                     deriving(Ord, Eq, Show)

-- | The tree-view building monad contains local state remembering
-- which nodes have already been incorporated into the tree, so we
-- don't display them more than once.
type BuildTreeView = State.State (Set.Set Solution)
unfoldSuccessor :: Successor -> BuildTreeView (TreeViewEntry, [Successor])
unfoldSuccessor (Successor step choice) =
    do seen <- State.get
       let sol = stepSol step
       (if sol `Set.member` seen
        then return (AlreadyGeneratedStep sol choice, [])
        else do State.put $ Set.insert sol seen
                return (Step step choice, stepSuccessors step))
unfoldSuccessor (Unprocessed sol choice) = return (NoStep sol choice, [])

logFileToTree :: LogFile -> Forest TreeViewEntry
logFileToTree log = case processingSteps log of
                      [] -> [Node (Error "Dependencies were not resolved.") []]
                      (root:rest) -> State.evalState (makeWholeTree root rest) Set.empty
    where makeTree :: (ProcessingStep -> TreeViewEntry) -> ProcessingStep -> BuildTreeView (Maybe (Tree TreeViewEntry))
          makeTree f root =
              do seen <- State.get
                 let rootSol = stepSol root
                 (if rootSol `Set.member` seen
                  then return $ Nothing
                  else do State.put $ Set.insert rootSol seen
                          let rootSucc = stepSuccessors root
                          succ <- unfoldForestM unfoldSuccessor rootSucc
                          return $ Just $ Node (f root) succ)
          makeWholeTree :: ProcessingStep -> [ProcessingStep] -> BuildTreeView (Forest TreeViewEntry)
          makeWholeTree root rest =
              do first <- makeTree Root root
                 -- This is always the first node we
                 -- process, so it should never
                 -- appear in "seen".
                 when (not $ isJust first) (error "Internal error: how can the root be seen already?")
                 rest' <- sequence $ map (makeTree makeOrphanedTree) rest
                 return $ catMaybes (first:rest')
          makeOrphanedTree root = Step root Unknown

choiceText :: LinkChoice -> String
choiceText (LinkChoice choice) = show choice -- TODO: custom choice rendering
choiceText Unknown = "(...)"

-- Column definitions for tree view entries.
entryColumnText :: TreeViewEntry -> String
entryColumnText (Root sol) = "Root"
entryColumnText (Step sol choice) = choiceText choice
entryColumnText (AlreadyGeneratedStep sol choice) = ("Already seen: " ++ choiceText choice)
entryColumnText (NoStep sol choice) = choiceText choice
entryColumnText (Error err) = err

renderTreeView :: LogFile -> IO (TreeStore TreeViewEntry)
renderTreeView lf = do let tree = logFileToTree lf
                       model <- (treeStoreNew tree)
                       treeModelSetColumn model (makeColumnIdString treeViewDisplayColumnText) entryColumnText
                       return model

setLog :: LogFile -> VisM ()
setLog lf =
    do ctx <- ask
       treeView <- getTreeView
       treeViewColInfo <- getTreeViewColumnInfo
       logView <- getLogView
       liftIO $ do -- Update the UI for the new log file.
                   rawLogViewBuffer <- textViewGetBuffer logView
                   textBufferSetText rawLogViewBuffer ""
                   model <- renderTreeView lf
                   -- Set up column bindings.
                   cellLayoutSetAttributes (treeViewTextColumn treeViewColInfo)
                                           (treeViewTextRenderer treeViewColInfo)
                                           model
                                           (\row -> [cellText := entryColumnText row])
                   -- Store information about the loaded file for later
                   -- use.
                   treeViewSetModel treeView model
                   -- Add the columns to the tree-view.
                   treeViewAppendColumn treeView (treeViewTextColumn treeViewColInfo)
                   -- Store the new log file information.
                   writeIORef (loadedFile ctx) (Just LoadedLogFile { logFile = lf,
                                                                     treeViewStore = model })


-- | Load a widget from the Glade file by name.
loadXmlWidget :: WidgetClass a => String -> (GObject -> a) -> IO (GladeXML, a)
loadXmlWidget widgetName cast =
    do x <- xmlNewWithRootAndDomain xmlFilename (Just widgetName) Nothing
       case x of
         Nothing -> error ("Can't load " ++ (show xmlFilename))
         Just xml -> do w <- xmlGetWidget xml cast widgetName
                        return (xml, w)

-- | Load the main window from the Glade file.
loadMainWindowXML :: IO (GladeXML, Window)
loadMainWindowXML = loadXmlWidget "main_window" castToWindow

-- | Load the About box from the Glade file.
loadAboutBoxXML :: IO (GladeXML, AboutDialog)
loadAboutBoxXML = loadXmlWidget "about_box" castToAboutDialog

-- | Load the loading progress window from the Glade file.
loadLoadingProgressXML :: IO (GladeXML, Window)
loadLoadingProgressXML = loadXmlWidget "window_load_progress" castToWindow

setupMainWindow :: GladeXML -> IO SourceView
setupMainWindow xml =
    do rightPane <- xmlGetWidget xml castToHPaned "hpaned2"
       sourceView <- sourceViewNew
       sourceViewSetShowLineNumbers sourceView True
       textViewSetEditable sourceView False
       panedAdd2 rightPane sourceView
       return sourceView

createMainWindowColumns :: GladeXML -> MainM (TreeViewColumnInfo, ChronologicalViewColumnInfo)
createMainWindowColumns xml =
    liftIO $ do tv <- xmlGetWidget xml castToTreeView "tree_view_search_tree"
                chrontv <- xmlGetWidget xml castToTreeView "tree_view_search_history"
                textRenderer <- cellRendererTextNew
                textCol      <- treeViewColumnNew
                cellLayoutPackEnd textCol textRenderer True
                return (TreeViewColumnInfo { treeViewTextColumn = textCol,
                                             treeViewTextRenderer = textRenderer },
                        ChronologicalViewColumnInfo)

newCtx :: GladeXML -> MainM VisualizeContext
newCtx xml =
    do mainCtx <- ask
       (treeViewColInf, chronViewColumnInf) <- createMainWindowColumns xml
       liftIO $ do sourceView <- setupMainWindow xml
                   tv <- xmlGetWidget xml castToTreeView "tree_view_search_tree"
                   chrontv <- xmlGetWidget xml castToTreeView "tree_view_search_history"
                   lf <- newIORef Nothing
                   return VisualizeContext { treeView = tv,
                                             treeViewColumnInfo = treeViewColInf,
                                             chronologicalView = chrontv,
                                             chronologicalViewColumnInfo = chronViewColumnInf,
                                             logView = sourceView,
                                             loadedFile = lf,
                                             mainContext = mainCtx }



newMainWindow :: MainM (GladeXML, VisualizeContext)
newMainWindow =
    do mainContext <- ask
       (xml, mainWindow) <- liftIO $ loadMainWindowXML
       ctx <- newCtx xml
       mainWindowOpened
       liftIO $ on mainWindow deleteEvent (liftIO $ doMainWindowClosed mainContext)
       return (xml, ctx)
    where doMainWindowClosed mainContext = do runMain mainWindowClosed mainContext
                                              return True

milliToPicoseconds n = n * 1000000000

-- | Load the given log file in a background thread, displaying the
-- progress in the foreground thread.  When it's done, pop up a new
-- main window.
--
-- TODO: support displaying in an existing main window.
load :: FilePath -> MainM ()
load fn = do loadedFile <- liftIO $ do (xml, win)  <- loadLoadingProgressXML
                                       title       <- xmlGetWidget xml castToLabel "label_title"
                                       progressBar <- xmlGetWidget xml castToProgressBar "load_progress"
                                       status      <- xmlGetWidget xml castToLabel "label_statistics"
                                       -- Should be done in a separate thread but GHC/Gtk2HS
                                       -- suck for threaded GUI programming.  See
                                       --
                                       -- http://haskell.org/gtk2hs/archives/2005/07/24/writing-multi-threaded-guis/1/
                                       --
                                       -- for (very gory) details.
                                       labelSetText title ("Loading " ++ fn ++ "...")
                                       widgetShow win
                                       h   <- openFile fn ReadMode
                                       lastTime <- newIORef Nothing
                                       log <- loadLogFile h fn (showProgress progressBar lastTime)
                                       widgetDestroy win
                                       return log
             (xml, ctx) <- newMainWindow
             liftIO $ runVis (setLog loadedFile) ctx
             liftIO $ xmlGetWidget xml castToWindow "main_window" >>= widgetShowAll
             return ()
    where showProgress :: ProgressBar -> IORef (Maybe ClockTime) -> Integer -> Integer -> IO ()
          showProgress pb lastTime cur max =
              do oldf     <- progressBarGetFraction pb
                 currTime <- getClockTime
                 last     <- readIORef lastTime
                 writeIORef lastTime (Just currTime)
                 let updateInterval = TimeDiff { tdYear = 0,
                                                 tdMonth = 0,
                                                 tdDay = 0,
                                                 tdHour = 0,
                                                 tdMin = 0,
                                                 tdSec = 0,
                                                 tdPicosec = milliToPicoseconds 100 }
                     newf           = if max == 0 then 0 else ((fromInteger cur) / (fromInteger max))
                     longUpdate     = case last of
                                        Nothing -> True
                                        Just time -> diffClockTimes currTime time >= updateInterval
                     shouldUpdate   = longUpdate || newf == 1
                 when shouldUpdate (do progressBarSetFraction pb newf
                                       progressBarSetText pb (showFFloat (Just 1) (100 * newf) "" ++ "%")
                                       while (mainContextIteration mainContextDefault False) (return ()) ()
                                       return ())

main :: IO ()
main = do -- Gtk2Hs whines loudly if it gets loaded into a threaded
          -- runtime, but runhaskell always loads a threaded runtime,
          -- so we have to call this to be a script:
          unsafeInitGUIForThreadedRTS
          mainWindows <- newIORef 0
          mainLoop <- mainLoopNew Nothing False
          let mainLoopContext = MainLoopContext { numMainWindows = mainWindows,
                                                  mainLoop = mainLoop }
          args <- getArgs
          case args of
            [] -> do (xml, ctx) <- runMain newMainWindow mainLoopContext
                     mainWin <- xmlGetWidget xml castToWindow "main_window"
                     widgetShow (toWidget mainWin)
            [filename] -> runMain (load filename) mainLoopContext
            otherwise -> error "Too many arguments; expected at most one (the log file to load)."
          mainLoopRun mainLoop

