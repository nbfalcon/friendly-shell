{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Either
import Data.Maybe (isJust)
import Data.Text hiding (null)
import System.FriendlyShell.Eval
import System.FriendlyShell.Parser
import System.FriendlyShell.ShellCore
import Test.Hspec
import Text.Megaparsec

shellSimpleTest :: Parser a -> (a -> ShellMonad Text) -> Text -> Text -> IO ()
shellSimpleTest parse' eval what expect = do
    let parsed' = fromRight undefined $ runParser parse' "<stdin>" what
    (r, errors) <- runShell $ do
        e <- eval parsed'
        liftIO $ e `shouldBe` expect
    errors `shouldSatisfy` null
    r `shouldSatisfy` isJust

main :: IO ()
main = hspec $ do
    describe "${ arith... }" $ do
        it "${ 1 + 2 }" $ do
            shellSimpleTest parseAtom evalAtom' "${ 1 + 2 }" "3"