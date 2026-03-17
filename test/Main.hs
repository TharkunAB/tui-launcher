-- | Test suite for @tui-launcher@.
module Main (main) where

import Control.Exception (SomeException, try)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (
    createDirectoryIfMissing,
    findExecutable,
    getTemporaryDirectory,
 )
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import TuiLauncher.App qualified as Launcher
import TuiLauncher.Config (defaultConfigText, loadResolvedConfig)
import TuiLauncher.Types (
    AppConfig (..),
    LaunchSpec (..),
    ResolvedConfig (..),
    ResolvedEntry (..),
    ShellConfig (..),
 )
import TuiSpec

-- | Run the test suite.
main :: IO ()
main = defaultMain tests

-- | Full test tree.
tests :: TestTree
tests =
    testGroup
        "tui-launcher"
        [ pureTests
        , tuiSpecSmoke
        ]

-- | Unit-style coverage for config loading and launch behavior.
pureTests :: TestTree
pureTests =
    testGroup
        "pure"
        [ testCase "default config mentions starter entries" $ do
            let expectedNames = ["Shell", "nvim", "Tmux", "Codex", "Claude"]
            mapM_ (\name -> assertBool ("missing " <> T.unpack name) (name `T.isInfixOf` defaultConfigText)) expectedNames
        , testCase "default config keeps extra starter entries commented out" $ do
            let commentedEntries = ["# name = \"nvim\"", "# name = \"Tmux\"", "# name = \"Codex\"", "# name = \"Claude\""]
            mapM_ (\entryLine -> assertBool ("missing commented entry " <> T.unpack entryLine) (entryLine `T.isInfixOf` defaultConfigText)) commentedEntries
        , testCase "default config uses multiline command strings" $
            assertBool "default config should use TOML multiline strings for commands" ("command = '''" `T.isInfixOf` defaultConfigText)
        , testCase "default config enables only Shell" $ do
            tempDir <- getTemporaryDirectory
            let configDir = tempDir </> "tui-launcher-default-config"
                configPath = configDir </> "config.toml"
            createDirectoryIfMissing True configDir
            TIO.writeFile configPath defaultConfigText
            resolved <- loadResolvedConfig (Just configPath)
            let entryNames = fmap resolvedName . NonEmpty.toList . configEntries . resolvedConfig $ resolved
            entryNames @?= ["Shell"]
        , testCase "default config includes nvim working-dir" $
            assertBool "default config should set ~/Code for nvim" ("working-dir = \"~/Code\"" `T.isInfixOf` defaultConfigText)
        , testCase "empty shell program is rejected during config load" $ do
            tempDir <- getTemporaryDirectory
            let configDir = tempDir </> "tui-launcher-empty-shell"
                configPath = configDir </> "config.toml"
            createDirectoryIfMissing True configDir
            TIO.writeFile configPath $
                T.unlines
                    [ "[[entries]]"
                    , "name = \"Broken\""
                    , "command = \"printf 'nope\\n'\""
                    , "shell-program = \"   \""
                    ]
            result <- try (loadResolvedConfig (Just configPath)) :: IO (Either SomeException ResolvedConfig)
            case result of
                Left _ -> pure ()
                Right _ -> assertFailure "expected config load to fail for empty shell-program"
        , testCase "empty working-dir is rejected during config load" $ do
            tempDir <- getTemporaryDirectory
            let configDir = tempDir </> "tui-launcher-empty-working-dir"
                configPath = configDir </> "config.toml"
            createDirectoryIfMissing True configDir
            TIO.writeFile configPath $
                T.unlines
                    [ "[[entries]]"
                    , "name = \"Broken\""
                    , "command = \"printf 'nope\\n'\""
                    , "working-dir = \"   \""
                    ]
            result <- try (loadResolvedConfig (Just configPath)) :: IO (Either SomeException ResolvedConfig)
            case result of
                Left _ -> pure ()
                Right _ -> assertFailure "expected config load to fail for empty working-dir"
        , testCase "launch converts missing working-dir into a clean exit failure" $ do
            result <-
                try $
                    Launcher.launch
                        LaunchSpec
                            { launchCommand = "printf 'never runs\\n'"
                            , launchWorkingDir = Just "/tmp/tui-launcher-does-not-exist"
                            , launchShell =
                                ShellConfig
                                    { shellProgram = "/bin/sh"
                                    , shellLogin = False
                                    }
                            } ::
                    IO (Either ExitCode ())
            case result of
                Left (ExitFailure 1) -> pure ()
                Left other -> assertFailure ("unexpected exit code: " <> show other)
                Right _ -> assertFailure "expected launch to fail for a missing working-dir"
        ]

-- | End-to-end PTY smoke test for the built executable.
tuiSpecSmoke :: TestTree
tuiSpecSmoke =
    tuiTest
        defaultRunOptions
            { timeoutSeconds = 15
            , artifactsDir = "artifacts/tui-launcher-smoke"
            }
        "launcher starts and executes selected entry"
        $ \tui -> do
            tempDir <- getTemporaryDirectory
            let configDir = tempDir </> "tui-launcher-test"
                configPath = configDir </> "config.toml"
            createDirectoryIfMissing True configDir
            exePath <- findExecutable "tui-launcher"
            case exePath of
                Nothing ->
                    fail "Could not find tui-launcher executable in PATH"
                Just binaryPath -> do
                    TIO.writeFile configPath launcherConfig
                    launch tui (app binaryPath ["--config", configPath])
                    waitForText tui (Exact "tui-launcher")
                    waitForText tui (Exact "Shell")
                    press tui Enter
                    waitForText tui (Exact "READY")
                    sendLine tui "exit"
  where
    launcherConfig =
        T.unlines
            [ "[theme]"
            , "name = \"github-dark\""
            , ""
            , "[layout]"
            , "tile-width = 20"
            , "tile-height = 20"
            , "tile-spacing = 1"
            , ""
            , "[[entries]]"
            , "name = \"Shell\""
            , "command = '''"
            , "printf 'READY\\n'; exec sh"
            , "'''"
            ]
