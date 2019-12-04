module Main where

import           Data.List           ((\\))
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Environment  (getArgs)


import qualified Command             as C
import qualified Query               as Q
import           Response

main2 :: IO ()
main2 = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch args = case args \\ options of
  ("create"   : args) -> C.handle rtype $ "create" : args
  ("add"      : args) -> C.handle rtype $ "create" : args
  ("update"   : args) -> C.handle rtype $ "update" : args
  ("edit"     : args) -> C.handle rtype $ "update" : args
  ("replace"  : args) -> C.handle rtype $ "replace" : args
  ("start"    : args) -> C.handle rtype $ "start" : args
  ("stop"     : args) -> C.handle rtype $ "stop" : args
  ("toggle"   : args) -> C.handle rtype $ "toggle" : args
  ("done"     : args) -> C.handle rtype $ "done" : args
  ("delete"   : args) -> C.handle rtype $ "delete" : args
  ("remove"   : args) -> C.handle rtype $ "remove" : args
  ("context"  : args) -> C.handle rtype $ "context" : args

  ("list"     : args) -> Q.handle rtype $ "list" : args
  ("show"     : args) -> Q.handle rtype $ "show" : args
  ("wtime"    : args) -> Q.handle rtype $ "worktime" : args
  ("worktime" : args) -> Q.handle rtype $ "worktime" : args

  (command    : _   ) -> printErr rtype $ command ++ ": command not found"
  -- if the user types no command, let's offer them some help
  _                   -> Q.handle rtype $ "help" : args
 where
  options = ["--json"]
  rtype   = getResponseType args

-- ** optparse-applicative rewrite ** --


data Input
  = CreateC String [String]
  | ShowC Int

createCmd :: Parser Input
createCmd = CreateC <$> strOption
  (  long "create"
  <> short 'c'
  <> help "Create a task with a <description> <+tag1> <+tag2>..." )

showCmd :: Parser Input
showCmd = ShowC <$> strOption
  (  long "show"
  <> short 's'
  <> metavar "Task ID"
  <> help "Show a Task with the ID N" )


input :: Parser Input
input = createCmd <|> showCmd

-- data Sample = Sample
--   { hello      :: String
--   , quiet      :: Bool
--   , enthusiasm :: Int }

-- sample :: Parser Sample
-- sample = Sample
--       <$> strOption
--           ( long "hello"
--          <> metavar "TARGET"
--          <> help "Target for the greeting" )
--       <*> switch
--           ( long "quiet"
--          <> short 'q'
--          <> help "Whether to be quiet" )
--       <*> option auto
--           ( long "enthusiasm"
--          <> help "How enthusiastically to greet"
--          <> showDefault
--          <> value 1
--          <> metavar "INT" )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Input -> IO ()
greet (CreateC desc tags) = putStrLn $ "Create task with desc " ++ desc
greet (ShowC taskId)      = putStrLn $ "Show task with ID" ++ (show taskId)
greet _                   = return ()
