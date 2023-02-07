module System.FriendlyShell.Eval (EvalAST (..)) where

import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.FriendlyShell.AST
import System.FriendlyShell.ShellCore
import System.IO
import System.IO.Error (tryIOError)
import System.Process (CreateProcess (std_out), ProcessHandle, StdStream (CreatePipe), createProcess, proc, waitForProcess)
import Control.Arrow
import Data.Text (Text)
import Control.Monad

type Process = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

getProcessHandle :: Process -> ProcessHandle
getProcessHandle (_, _, _, h) = h

getProcessStdout :: Process -> Handle
getProcessStdout (_, Just out, _, _) = out
getProcessStdout (_, Nothing, _, _) = error "stdout should exist: use CreatePipe in createProcess"

exitCodeAsInt :: ExitCode -> Int
exitCodeAsInt ExitSuccess = 0
exitCodeAsInt (ExitFailure code) = code

finishProcess :: ProcessHandle -> IO Int
finishProcess subProc = do
    exitCode <- exitCodeAsInt <$> waitForProcess subProc
    hFlush stdout
    hFlush stderr
    pure exitCode

runProcessWithExitCode :: FilePath -> [String] -> IO Int
runProcessWithExitCode cmd args = do
    subProc <- getProcessHandle <$> createProcess (proc cmd args)
    finishProcess subProc

runProcessForStdout :: FilePath -> [String] -> IO (Int, Text)
runProcessForStdout cmd args = do
    (subProc, out) <- (getProcessHandle &&& getProcessStdout) <$> createProcess (proc cmd args){std_out = CreatePipe}
    content <- T.hGetContents out
    exitCode <- finishProcess subProc
    pure (exitCode, content)

liftIOForShell :: IO a -> ShellMonad a
liftIOForShell = either (\e -> newError (show e) >> failShell) pure <=< (liftIO . tryIOError)

class EvalAST a where
    evalAST :: a -> ShellMonad ()

evalComponent :: AComponent -> ShellMonad Text
evalComponent (CConstant c) = pure c
evalComponent (CVarRef v) = getVar v
evalComponent (CExecuteSubcommandForStdout cmd args) = do
    cmd' <- evalAtom cmd
    args' <- mapM evalAtom args
    (result, out) <- liftIOForShell $ runProcessForStdout cmd' args'
    updateExitCode result
    pure out

evalAtom' :: SAtom -> ShellMonad Text
evalAtom' (AString components) = T.concat <$> mapM evalComponent components

evalAtom :: SAtom -> ShellMonad String
evalAtom = (T.unpack <$>) . evalAtom'

instance EvalAST SStatement where
    evalAST (SExecuteShell cmd args) = do
        cmd' <- evalAtom cmd
        args' <- mapM evalAtom args
        exitCode <- liftIOForShell $ runProcessWithExitCode cmd' args'
        updateExitCode exitCode
    evalAST (SAssignVar var toWhat) = do
        toWhat' <- evalAtom' toWhat
        updateVar var toWhat'

instance EvalAST SModule where
    evalAST (SModule statements) = mapM_ evalAST statements