module System.FriendlyShell.AST where

import Data.Text (Text)

data PipeTo = PipeExec ExecuteCommand | PipeFile SAtom
data ExecuteCommand = ExecuteCommand { shProgram :: SAtom, shArgs :: [SAtom], pipeTo :: Maybe PipeTo }

data AComponent
    = CConstant Text
    | CVarRef String
    | CArithExpr ArithExpr
    | CExecuteSubcommandForStdout ExecuteCommand

data SAtom = AString [AComponent]

data SStatement
    = SExecuteShell ExecuteCommand
    | SAssignVar String SAtom

data SModule = SModule [SStatement]

data ArithExpr
    = FConstant Text
    | FVarRef String
    | FForStdout ExecuteCommand
    | FCatExpr ArithExpr ArithExpr
    | FAddExpr ArithExpr ArithExpr
    | FSubExpr ArithExpr ArithExpr
    | FDivExpr ArithExpr ArithExpr
    | FModExpr ArithExpr ArithExpr
    | FMulExpr ArithExpr ArithExpr
    | FUnegExpr ArithExpr