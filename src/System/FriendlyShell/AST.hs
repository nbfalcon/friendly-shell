module System.FriendlyShell.AST where

import Data.Text (Text)

data ExecuteCommand = ExecuteCommand { shProgram :: SAtom, shArgs :: [SAtom], pipeTo :: Maybe ExecuteCommand }

data AComponent
    = CConstant Text
    | CVarRef String
    | CExecuteSubcommandForStdout ExecuteCommand

data SAtom = AString [AComponent]

data SStatement
    = SExecuteShell ExecuteCommand
    | SAssignVar String SAtom

data SModule = SModule [SStatement]