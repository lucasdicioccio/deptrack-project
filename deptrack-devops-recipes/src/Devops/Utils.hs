{-# LANGUAGE BangPatterns #-}

module Devops.Utils (
    checkExitCodeAndStdout
  , checkBinaryExitCodeAndStdout
  , checkExitCode
  , blindRun , blindRunPreinstalled , blindRunInDir, blindRunWithEnv
  , retryWithBackoff
  , replaceInFile
  ) where

import           Control.Concurrent      (threadDelay)
import           Control.Exception       (IOException, catch)
import           Control.Monad           (void)
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Devops.Binary
import           Devops.Base
import           System.Environment      (getEnvironment)
import           System.Exit             (ExitCode (..))
import qualified System.IO.Strict        as Strict
import           System.Process          (CreateProcess (..), proc,
                                          readCreateProcess, readProcess,
                                          readProcessWithExitCode)

checkExitCode :: FilePath -> [String] -> String -> IO CheckResult
checkExitCode = runAndCheckResult (const True)

checkExitCodeAndStdout :: (String -> Bool) -> FilePath -> [String] -> String -> IO CheckResult
checkExitCodeAndStdout = runAndCheckResult

checkBinaryExitCodeAndStdout :: (String -> Bool) -> Binary a -> [String] -> String -> IO CheckResult
checkBinaryExitCodeAndStdout f (Binary c) = runAndCheckResult f c

runAndCheckResult :: (String -> Bool)
                  -- ^ check function
                  -> FilePath
                  -- ^ binary path
                  -> [String]
                  -- ^ command args
                  -> String
                  -- ^ stdin
                  -> IO CheckResult
runAndCheckResult resultCheck command args input = do
  (code,output,_) <- readProcessWithExitCode command args input
  let !ret  = if code == ExitSuccess
              then fromBool $ resultCheck output
              else Failure (show code)
  return ret

blindRun :: Binary a -> [String] -> String -> IO ()
blindRun (Binary c) args input =
  void $ catch (readProcess c args input)
               (\e -> print ("caught", e :: IOException) >> return "")

blindRunPreinstalled :: FilePath -> [String] -> String -> IO ()
blindRunPreinstalled c args input =
  void $ catch (readProcess c args input)
               (\e -> print ("caught", e :: IOException) >> return "")

blindRunInDir :: Binary a -> FilePath -> [String] -> String -> IO ()
blindRunInDir (Binary c) path args input =
  void $ catch (readCreateProcess (proc c args) { cwd = Just path} input)
               (\e -> print ("caught", e :: IOException) >> return "")

-- |Run blindly a process, extending current environment with given key/value pairs
blindRunWithEnv :: Binary a -> [String] -> String -> [(String,String)] -> IO ()
blindRunWithEnv (Binary c) args input extendedEnv = do
  envs <- getEnvironment
  void $ catch (readCreateProcess (proc c args) { env = Just $ extendedEnv ++ envs } input)
               (\e -> print ("caught", e :: IOException) >> return "")

retryWithBackoff :: Int -> Int -> OpCheck -> OpCheck
retryWithBackoff _ 0 _ = return $ Failure "attempts exhausted"
retryWithBackoff delay n check = do
  print ("checking again", n)
  x <- check
  case x of
     Success -> return Success
     _ -> threadDelay (delay * 1000000) >> retryWithBackoff (2 * delay) (n - 1) check

replaceInFile :: FilePath -> Text -> Text -> IO ()
replaceInFile path from to = do
  dat <- Strict.readFile path
  let out = Text.replace from to (convertString dat)
  writeFile path (convertString out)

