module Query where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Duration
import           Data.Fixed
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Text.PrettyPrint.Boxes
import           Text.Read

import           Event
import           Response
import           State
import qualified Store
import           Task
import           Utils

data Query
  = ShowTasks [String]
  | ShowTask Id [String]
  | ShowWtime [String]
  | Error String String
  | NoCommand
  deriving (Show)

handle :: ResponseType -> [String] -> IO ()
handle rtype args = do
  events <- Store.readAll
  let state = State.applyAll events
  let query = parseArgs args
  execute rtype state events query

parseArgs :: [String] -> Query
parseArgs args = case args of
  ("list"          : args) -> ShowTasks args
  ("worktime"      : args) -> ShowWtime args
  ("help"          : args) -> NoCommand
  ("show" : number : args) -> case readMaybe number of
    Nothing     -> Query.Error "show" "task not found"
    Just number -> ShowTask number args

execute :: ResponseType -> State -> [Event] -> Query -> IO ()
execute rtype state events query = case query of
  ShowTasks _args -> do
    now <- getCurrentTime
    let fByTags = filterByTags $ _context state
    let fByDone = filterByDone $ _showDone state
    let tasks   = mapWithWtime now . fByTags . fByDone $ _tasks state
    case rtype of
      JSON -> printTasks JSON tasks
      Text -> do
        let ctx    = [ "done" | _showDone state ] ++ _context state
        let ctxStr = if null ctx then "" else " [" ++ unwords ctx ++ "]"
        putStrLn $ "unfog: list" ++ ctxStr
        printTasks Text tasks

  ShowTask id args -> do
    now <- getCurrentTime
    let fByTags   = filterByTags $ _context state
    let fByDone   = filterByDone $ _showDone state
    let fByNumber = findById id
    let maybeTask = fByNumber . fByTags . fByDone $ _tasks state
    case maybeTask of
      Nothing   -> printErr rtype "show: task not found"
      Just task -> printTask rtype $ task { _wtime = getTotalWtime now task }

  ShowWtime args -> do
    now <- getCurrentTime
    let tags  = filter startsByPlus args
    let ids   = map _id $ filterByTags args $ _tasks state
    let tasks = filterByIds ids $ mapWithWtime now $ _tasks state
    let wtime = getWtimePerDay now tasks
    let ctx = if null args then "global" else "for [" ++ unwords tags ++ "]"
    printWtime rtype ("unfog: wtime " ++ ctx) wtime

  NoCommand -> do
    now <- getCurrentTime
    let fByTags = filterByTags $ _context state
    let fByDone = filterByDone $ _showDone state
    let tasks   = mapWithWtime now . fByTags . fByDone $ _tasks state
    case rtype of
      JSON -> printTasks JSON tasks
      Text -> do
        let ctx    = [ "done" | _showDone state ] ++ _context state
        let ctxStr = if null ctx then "" else " [" ++ unwords ctx ++ "]"
        putStrLn $ "unfog: list" ++ ctxStr
        printTasks Text tasks

  Query.Error command message -> printErr rtype $ command ++ ": " ++ message
