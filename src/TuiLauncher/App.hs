module TuiLauncher.App (
    main,
    launch,
) where

import Control.Exception (SomeException, displayException, try)
import Data.Text qualified as T
import Options.Applicative
import System.Directory (setCurrentDirectory)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import TuiLauncher.Config (loadResolvedConfig)
import TuiLauncher.Types
import TuiLauncher.UI (runUi)

main :: IO ()
main = do
    options <- execParser parserInfo
    resolved <- loadResolvedConfig (appConfigOverride options)
    selected <- runUi (resolvedConfig resolved)
    maybe (pure ()) launch selected

parserInfo :: ParserInfo AppOptions
parserInfo =
    info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Launch shell commands from a Brick-based menu")

optionsParser :: Parser AppOptions
optionsParser =
    AppOptions
        <$> optional
            ( strOption
                ( long "config"
                    <> metavar "PATH"
                    <> help "Override the default config path"
                )
            )

launch :: LaunchSpec -> IO ()
launch LaunchSpec{..} = do
    let shellArgs = (if shellLogin launchShell then ["-lc"] else ["-c"]) <> [T.unpack launchCommand]
    result <- try @SomeException do
        maybe (pure ()) setCurrentDirectory launchWorkingDir
        _ <- executeFile (shellProgram launchShell) True shellArgs Nothing
        pure ()
    case result of
        Left err -> do
            hPutStrLn stderr ("Failed to launch command: " <> displayException err)
            exitFailure
        Right _ ->
            pure ()
