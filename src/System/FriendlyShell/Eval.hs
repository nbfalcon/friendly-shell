module System.FriendlyShell.Eval (EvalAST (..), evalAtom') where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.FriendlyShell.AST
import System.FriendlyShell.ShellCore
import System.IO
import System.Process

type Process = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

getProcessHandle :: Process -> ProcessHandle
getProcessHandle (_, _, _, h) = h

exitCodeAsInt :: ExitCode -> Int
exitCodeAsInt ExitSuccess = 0
exitCodeAsInt (ExitFailure code) = code

data Redirects = Redirects {stdinH :: !StdStream, stdoutH :: !StdStream}

runExecuteCommand :: ExecuteCommand -> Redirects -> ShellMonad ()
runExecuteCommand ExecuteCommand{shProgram, shArgs} Redirects{stdinH, stdoutH} = do
    cmd <- evalAtom shProgram
    args <- mapM evalAtom shArgs
    p <- liftIOForShell $ createProcess_ cmd (proc cmd args){std_in = stdinH, std_out = stdoutH}
    exitCode <- fmap exitCodeAsInt $ liftIOForShell $ waitForProcess $ getProcessHandle p
    updateExitCode exitCode
    pure ()
runExecuteCommand ExecPipe{executeMe, redirectStdout = (PipeFile f)} Redirects{stdinH} = do
    f' <- evalAtom f
    redir <- liftIOForShell $ openFile f' ReadWriteMode
    runExecuteCommand executeMe Redirects{stdinH, stdoutH = UseHandle redir}
runExecuteCommand ExecPipe{executeMe, redirectStdout = PipeStdout} r = runExecuteCommand executeMe r
runExecuteCommand ExecPipe{executeMe, redirectStdout = (PipeExec pipeToExec)} Redirects{stdinH, stdoutH} = do
    (readEnd, writeEnd) <- liftIOForShell createPipe
    forkShell $ do
        runExecuteCommand executeMe Redirects{stdinH, stdoutH = UseHandle writeEnd}
        -- FIXME: this is a resource like on error
        liftIOForShell $ hClose writeEnd
    runExecuteCommand pipeToExec Redirects{stdinH = UseHandle readEnd, stdoutH}
    -- FIXME: this can be closed earlier (between wait and createProcess)
    liftIOForShell $ hClose readEnd
runExecuteCommand (ExecThen lhs rhs) r = do
    _ <- runExecuteCommand lhs r
    runExecuteCommand rhs r
runExecuteCommand (ExecAnd lhs rhs) r = do
    _ <- runExecuteCommand lhs r
    code <- getLastExitCode
    when (code == 0) $ runExecuteCommand rhs r
runExecuteCommand (ExecOr lhs rhs) r = do
    _ <- runExecuteCommand lhs r
    code <- getLastExitCode
    unless (code == 0) $ runExecuteCommand rhs r

runExecuteCommandForStdout :: ExecuteCommand -> ShellMonad Text
runExecuteCommandForStdout ex = do
    (readEnd, writeEnd) <- liftIOForShell createPipe
    runExecuteCommand ex Redirects{stdinH = Inherit, stdoutH = UseHandle writeEnd}
    liftIOForShell $ do
        c <- T.hGetContents readEnd
        hClose readEnd
        hClose writeEnd
        pure c

class EvalAST a where
    evalAST :: a -> ShellMonad ()

evalArithExpr :: ArithExpr -> ShellMonad Text
evalArithExpr (FConstant c) = pure c
evalArithExpr (FVarRef name) = getVar name
evalArithExpr (FForStdout ex) = runExecuteCommandForStdout ex
evalArithExpr (FCatExpr lhs rhs) = T.append <$> evalArithExpr lhs <*> evalArithExpr rhs
evalArithExpr (FAddExpr lhs rhs) = tOp2 (+) <$> evalArithExpr lhs <*> evalArithExpr rhs
evalArithExpr (FSubExpr lhs rhs) = tOp2 (-) <$> evalArithExpr lhs <*> evalArithExpr rhs
evalArithExpr (FMulExpr lhs rhs) = tOp2 (*) <$> evalArithExpr lhs <*> evalArithExpr rhs
evalArithExpr (FDivExpr lhs rhs) = tOp2 div <$> evalArithExpr lhs <*> evalArithExpr rhs
evalArithExpr (FModExpr lhs rhs) = tOp2 mod <$> evalArithExpr lhs <*> evalArithExpr rhs
evalArithExpr (FUnegExpr rhs) = tOp1 negate <$> evalArithExpr rhs

tOp1 :: (Integer -> Integer) -> (Text -> Text)
tOp1 f = T.pack . show . f . read . T.unpack

tOp2 :: (Integer -> Integer -> Integer) -> (Text -> Text -> Text)
tOp2 f l r = T.pack $ show $ f (read $ T.unpack l) (read $ T.unpack r)

evalComponent :: AComponent -> ShellMonad Text
evalComponent (CConstant c) = pure c
evalComponent (CVarRef v) = getVar v
evalComponent (CExecuteSubcommandForStdout ex) = runExecuteCommandForStdout ex
evalComponent (CArithExpr e) = evalArithExpr e

evalAtom' :: SAtom -> ShellMonad Text
evalAtom' (AString components) = T.concat <$> mapM evalComponent components

evalAtom :: SAtom -> ShellMonad String
evalAtom = (T.unpack <$>) . evalAtom'

instance EvalAST SStatement where
    evalAST (SExecuteShell ex) = runExecuteCommand ex Redirects{stdinH = Inherit, stdoutH = Inherit}
    evalAST (SAssignVar var toWhat) = do
        toWhat' <- evalAtom' toWhat
        updateVar var toWhat'

instance EvalAST SModule where
    evalAST (SModule statements) = mapM_ evalAST statements