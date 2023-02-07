module System.FriendlyShell.ShellCore (ShellMonad, runShell, updateExitCode, newRunCmdError, newError, failShell, getVar, updateVar, withErrors) where

import Control.Applicative (Alternative)
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Map qualified as M
import Data.Text qualified as T

data ShellState = ShellState
    { lastExitCode :: !Int
    , varTable :: M.Map String T.Text
    }
data ShellError = RunCommandError FilePath [String] | AnyError String

instance Show ShellError where
    show (RunCommandError cmd args) = "Failed to run: " ++ unwords (cmd : args)
    show (AnyError e) = e

newtype ShellMonad a = ShellMonad {runShellM :: MaybeT (StateT ShellState (WriterT [ShellError] IO)) a} deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

newRunCmdError :: FilePath -> [String] -> ShellMonad ()
newRunCmdError cmd args = ShellMonad $ tell [RunCommandError cmd args]

newError :: String -> ShellMonad ()
newError s = ShellMonad $ tell [AnyError s]

failShell :: ShellMonad a
failShell = mzero

updateExitCode :: Int -> ShellMonad ()
updateExitCode newExit = ShellMonad $ modify' $ \s -> s{lastExitCode = newExit}

updateVar :: String -> T.Text -> ShellMonad ()
updateVar var toValue = ShellMonad $ modify' $ \s@ShellState{varTable} -> s{varTable = M.insert var toValue varTable}

withErrors :: ShellMonad () -> ShellMonad [ShellError]
withErrors action = ShellMonad $ do
    ((), errors) <- (listen . runShellM) action
    pure errors

getVar :: String -> ShellMonad T.Text
getVar "_" = T.pack . show <$> ShellMonad (gets lastExitCode)
getVar genericVar = do
    boundTo <- ShellMonad $ gets $ M.lookup genericVar . varTable
    maybe (newError ("Unknown variable: $" ++ genericVar) >> failShell) pure boundTo

runShell :: ShellMonad a -> IO (Maybe a, [ShellError])
runShell = runWriterT . flip evalStateT ShellState{lastExitCode = 0, varTable = M.empty} . runMaybeT . runShellM