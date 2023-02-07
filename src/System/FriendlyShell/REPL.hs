module System.FriendlyShell.REPL (read', runCmd, replLoop, replLoopIO) where

import Control.Applicative
import Control.Exception.Base
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.Text as T (Text)
import Data.Text.IO qualified as T
import GHC.IO.Exception (IOErrorType (EOF), IOException (IOError, ioe_type))
import System.FriendlyShell.Eval
import System.FriendlyShell.Parser
import System.FriendlyShell.ShellCore (ShellMonad, runShell, withErrors)
import System.IO
import System.IO.Error (tryIOError)
import Text.Megaparsec (errorBundlePretty, parse)

read' :: IO (Maybe Text)
read' = do
    putStr "$ "
    hFlush stdout
    nextLine <- tryIOError T.getLine
    case nextLine of
        Left IOError{ioe_type = EOF} -> pure Nothing
        Left anyOtherError -> throwIO anyOtherError
        Right text -> pure $ Just text

runCmd :: Text -> ShellMonad ()
runCmd line = case parsed of
    Left bundle -> liftIO $ putStrLn (errorBundlePretty bundle)
    Right statement -> do
        -- Don't fail the entire shell evaluation due to one error
        errors <- withErrors $ evalAST statement <|> pure ()
        liftIO $ mapM_ print errors
  where
    parsed = parse parseStatement "<stdin>" line

replLoop :: ShellMonad ()
replLoop = do
    input <- liftIO read'
    sequenceA_ $ input <&> (runCmd >=> const replLoop)

replLoopIO :: IO ()
replLoopIO = void $ runShell replLoop