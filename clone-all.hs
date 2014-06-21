{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Prelude
import System.Directory          (doesDirectoryExist)
import Control.Monad             (filterM, liftM)
import System.Exit               (ExitCode(..), exitFailure, exitSuccess)
import System.Process            (system)
import Control.Applicative       ((<$>), (<*>))
import Data.Monoid               (mempty, (<>))
import Data.Maybe                (fromMaybe)
import Github.Data               (Repo(..))
import Github.Repos              (userRepos, RepoPublicity(..))

import qualified Options.Applicative.Builder.Internal as X
import qualified Options.Applicative                  as O

data Options = Options { directory :: String
                       , user      :: String
                       } deriving Show

main :: IO ()
main = O.execParser (O.info (O.helper <*> options) mempty) >>= start

options :: O.Parser Options
options = Options
  <$> defStr "."             ( O.metavar "DIRECTORY"            <> O.help "Directory to clone everything into" )
  <*> O.strOption            ( O.short 'u' <> O.long "user"     <> O.help "Name of the user to clone the repos of.")

defStr :: String -> X.Mod X.ArgumentFields String -> O.Parser String
defStr a = def a . O.argument O.str

def :: a -> O.Parser a -> O.Parser a
def a = fmap (fromMaybe a) . O.optional

start :: Options -> IO ()
start opts = do
  putStrLn $ "Looking up repositories for " ++ (user opts) ++ " ..."

  d <- userRepos (user opts) All

  case d of
    Left  err   -> putStrLn $ show err
    Right repos -> cloneRepos repos opts


cloneRepos :: [Repo] -> Options -> IO ()
cloneRepos rawRepos opts = do
  putStrLn $ "Found " ++ (show $ length rawRepos) ++ " repositories for " ++ (user opts) ++ "."

  -- Filter the ones that already exist
  repos <- filterM (\r -> (liftM not) $ doesDirectoryExist $ ((directory opts) ++ "/" ++ (repoName r))) rawRepos

  putStrLn $ (show $ length repos) ++ " that you don't already have."

  returnCodes <- mapM (\r -> system $ "cd " ++ (directory opts) ++ " && git clone " ++ (repoSshUrl r)) repos
  let rs = all (\e -> e == ExitSuccess) returnCodes
  
  case rs
    of True   -> exitSuccess
       False  -> exitFailure
