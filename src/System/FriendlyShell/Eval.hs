module System.FriendlyShell.Eval (EvalAST (..)) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.FriendlyShell.AST
import System.FriendlyShell.ShellCore
import System.IO
import System.IO.Error (tryIOError)
import System.Process

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

readProcessStdout :: Process -> IO Text
readProcessStdout subProc = do
    let stdoutH = getProcessStdout subProc
    T.hGetContents stdoutH

data Redirects = Redirects {stdinH :: !StdStream, stdoutH :: !StdStream}

runExecuteCommand :: ExecuteCommand -> Redirects -> ShellMonad Process
runExecuteCommand ExecuteCommand{shProgram, shArgs, pipeTo} Redirects{stdinH, stdoutH} = do
    cmd <- evalAtom shProgram
    args <- mapM evalAtom shArgs
    p <- liftIOForShell $ createProcess (proc cmd args){std_out = if isNothing pipeTo then stdoutH else CreatePipe, std_in = stdinH}
    case pipeTo of
        Just subC -> runExecuteCommand subC Redirects{stdinH = UseHandle $ getProcessStdout p, stdoutH}
        _ -> pure p

liftIOForShell :: IO a -> ShellMonad a
liftIOForShell = either (\e -> newError (show e) >> failShell) pure <=< (liftIO . tryIOError)

class EvalAST a where
    evalAST :: a -> ShellMonad ()
    
evalComponent :: AComponent -> ShellMonad Text
evalComponent (CConstant c) = pure c
evalComponent (CVarRef v) = getVar v
evalComponent (CExecuteSubcommandForStdout ex) = do
    p <- runExecuteCommand ex Redirects{stdinH=Inherit, stdoutH=CreatePipe}
    out <- liftIOForShell $ readProcessStdout p
    exitCode <- liftIOForShell $ exitCodeAsInt <$> waitForProcess (getProcessHandle p)
    updateExitCode exitCode
    pure out

evalAtom' :: SAtom -> ShellMonad Text
evalAtom' (AString components) = T.concat <$> mapM evalComponent components

evalAtom :: SAtom -> ShellMonad String
evalAtom = (T.unpack <$>) . evalAtom'

instance EvalAST SStatement where
    evalAST (SExecuteShell ex) = do
        p <- runExecuteCommand ex Redirects{stdinH=Inherit, stdoutH=Inherit}
        exitCode <- liftIOForShell $ finishProcess $ getProcessHandle p
        updateExitCode exitCode
    evalAST (SAssignVar var toWhat) = do
        toWhat' <- evalAtom' toWhat
        updateVar var toWhat'

instance EvalAST SModule where
    evalAST (SModule statements) = mapM_ evalAST statements