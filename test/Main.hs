-- | End-to-end TUISpec coverage for @tui-launcher@.
module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (
    createDirectoryIfMissing,
    findExecutable,
    getTemporaryDirectory,
 )
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import TuiSpec

-- | Run the TUISpec test suite.
main :: IO ()
main = defaultMain tests

-- | Full test tree.
tests :: TestTree
tests =
    testGroup
        "tui-launcher"
        [launchesShellEntryFromExplicitConfig]

{- | Verify that @--config@ loads explicit entries, arrow-key navigation moves
selection, and choosing the @Shell@ entry drops into a real shell.
-}
launchesShellEntryFromExplicitConfig :: TestTree
launchesShellEntryFromExplicitConfig =
    tuiTest runOptions "launches shell from --config with arrow keys" $ \tui -> do
        (binaryPath, configPath) <- prepareConfig "tui-launcher-launch" launcherConfig
        launch tui (app binaryPath ["--config", configPath])
        waitForText tui (Nth 0 (Exact "tui-launcher"))
        waitForText tui (Nth 0 (Exact "Logs"))
        waitForText tui (Nth 0 (Exact "InteractiveShell"))
        waitForText tui (Nth 0 (Exact "Exit"))
        press tui ArrowRight
        press tui Enter
        sendLine tui "echo 'hello, world'"
        waitForText tui (Nth 0 (Exact "hello, world"))
        sendLine tui "exit"

-- | Shared TUISpec runtime options for integration tests.
runOptions :: RunOptions
runOptions =
    defaultRunOptions
        { timeoutSeconds = 15
        , terminalCols = 80
        , terminalRows = 24
        , artifactsDir = "artifacts/tui-launcher-smoke"
        }

{- | Write a config file for a test and return the launcher binary plus config
path.
-}
prepareConfig :: FilePath -> T.Text -> IO (FilePath, FilePath)
prepareConfig testName configText = do
    tempDir <- getTemporaryDirectory
    let configDir = tempDir </> testName
        configPath = configDir </> "config.toml"
    createDirectoryIfMissing True configDir
    TIO.writeFile configPath configText
    exePath <- findExecutable "tui-launcher"
    case exePath of
        Nothing -> fail "Could not find tui-launcher executable in PATH"
        Just binaryPath -> pure (binaryPath, configPath)

-- | Explicit launcher config used by the TUISpec tests.
launcherConfig :: T.Text
launcherConfig =
    T.unlines
        [ "[layout]"
        , "tile-width = 18"
        , "tile-height = 5"
        , "tile-spacing = 1"
        , ""
        , "[[entries]]"
        , "name = \"Logs\""
        , "command = \"printf 'not-a-shell\\\\n'; exec sh\""
        , "color = \"red\""
        , ""
        , "[[entries]]"
        , "name = \"InteractiveShell\""
        , "command = \"exec /bin/sh\""
        , "color = \"green\""
        ]
