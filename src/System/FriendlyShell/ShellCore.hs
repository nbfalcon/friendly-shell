module System.FriendlyShell.ShellCore (ShellMonad, initialShellState, runShell, updateExitCode, newRunCmdError, newError, failShell, getVar, updateVar, withErrors, getLastExitCode, liftIOForShell, forkShell) where

import Control.Applicative (Alternative)
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Map qualified as M
import Data.Text qualified as T
import System.IO.Error (tryIOError)

data ShellState = ShellState
    { lastExitCode :: !Int
    , varTable :: M.Map String T.Text
    }
initialShellState :: ShellState
initialShellState = ShellState{lastExitCode = 0, varTable = M.empty}
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
getLastExitCode :: ShellMonad Int
getLastExitCode = ShellMonad $ gets lastExitCode

updateVar :: String -> T.Text -> ShellMonad ()
updateVar var toValue = ShellMonad $ modify' $ \s@ShellState{varTable} -> s{varTable = M.insert var toValue varTable}

withErrors :: ShellMonad () -> ShellMonad [ShellError]
withErrors action = ShellMonad $ do
    ((), errors) <- (listen . runShellM) action
    pure errors

getVar :: String -> ShellMonad T.Text
getVar "_" = T.pack . show <$> getLastExitCode
getVar genericVar = do
    boundTo <- ShellMonad $ gets $ M.lookup genericVar . varTable
    maybe (newError ("Unknown variable: $" ++ genericVar) >> failShell) pure boundTo

liftIOForShell :: IO a -> ShellMonad a
liftIOForShell = either (\e -> newError (show e) >> failShell) pure <=< (liftIO . tryIOError)

forkShell :: ShellMonad () -> ShellMonad ()
forkShell subshell = do
    st' <- ShellMonad $ get
    liftIOForShell $ runShell' st' subshell
    pure ()

runShell' :: ShellState -> ShellMonad a -> IO (Maybe a, [ShellError])
runShell' i = runWriterT . flip evalStateT i . runMaybeT . runShellM 

runShell :: ShellMonad a -> IO (Maybe a, [ShellError])
runShell = runShell' initialShellState