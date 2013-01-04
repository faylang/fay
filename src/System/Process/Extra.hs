-- | Extra process functions.

module System.Process.Extra where

import System.Exit
import System.Process

-- | Read everything from a process, either failure or both stderr and stdout.
readAllFromProcess :: FilePath -> [String] -> String -> IO (Either String (String,String))
readAllFromProcess program flags input = do
  (code,out,err) <- readProcessWithExitCode program flags input
  return $ case code of
    ExitFailure _ -> Left err
    ExitSuccess   -> Right (err, out)
