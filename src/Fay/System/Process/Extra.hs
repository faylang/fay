-- | Extra process functions.

module Fay.System.Process.Extra where

import           System.Exit
import           System.Process

-- | Read from a process returning both std err and out.
readAllFromProcess :: FilePath -> [String] -> String -> IO (Either (String,String) (String,String))
readAllFromProcess program flags input = do
  (code,out,err) <- readProcessWithExitCode program flags input
  return $ case code of
    ExitFailure 127 -> Left ("cannot find executable " ++ program, "")
    ExitFailure _   -> Left (err, out)
    ExitSuccess     -> Right (err, out)
