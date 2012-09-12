-- | Extra process functions.

module System.Process.Extra where


import           System.Exit
import           System.IO
import           System.Process

-- | Read all stuff from a process.
readAllFromProcess :: FilePath -> String -> IO (Either String String)
readAllFromProcess program file = do
  (_,out,err,pid) <- runInteractiveProcess program [file] Nothing Nothing
  code <- waitForProcess pid
  case code of
    ExitSuccess -> fmap Right (hGetContents out)
    ExitFailure _ -> fmap Left (hGetContents err)

readAllFromProcess' :: FilePath -> [String] -> String -> IO (Either String (String,String))
readAllFromProcess' program flags input = do
  (code,out,err) <- readProcessWithExitCode program flags input
  return $ case code of
    ExitFailure _ -> Left err
    ExitSuccess   -> Right (err, out)
