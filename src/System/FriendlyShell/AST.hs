module System.FriendlyShell.AST where

import Data.Text (Text)

data AComponent
    = CConstant Text
    | CVarRef String
    | CExecuteSubcommandForStdout SAtom [SAtom]

data SAtom = AString [AComponent]

data SStatement
    = SExecuteShell SAtom [SAtom]
    | SAssignVar String SAtom

data SModule = SModule [SStatement]