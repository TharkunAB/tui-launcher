module TUILauncher.Config (
    defaultConfigText,
    loadResolvedConfig,
) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getHomeDirectory,
 )
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import TUILauncher.Types
import Toml qualified

defaultConfigText :: T.Text
defaultConfigText =
    T.unlines
        [ "[theme]"
        , "name = \"github-dark\""
        , ""
        , "[layout]"
        , "tile-width = 20"
        , "tile-height = 20"
        , "tile-spacing = 1"
        , ""
        , "# [shell]"
        , "# program = \"/bin/bash\""
        , "# login = false"
        , ""
        , "[[entries]]"
        , "name = \"Shell\""
        , "command = \"exec \\\"${SHELL:-/bin/sh}\\\"\""
        , ""
        , "[[entries]]"
        , "name = \"nvim\""
        , "command = \"nvim\""
        , "working-dir = \"~/Code\""
        , ""
        , "[[entries]]"
        , "name = \"Tmux\""
        , "command = \"tmux\""
        , "# working-dir = \"~/Code/project\""
        , "# shell-program = \"/bin/zsh\""
        , "# shell-login = true"
        , ""
        , "[[entries]]"
        , "name = \"Codex\""
        , "command = \"codex\""
        , ""
        , "[[entries]]"
        , "name = \"Claude\""
        , "command = \"claude\""
        ]

loadResolvedConfig :: Maybe FilePath -> IO ResolvedConfig
loadResolvedConfig overridePath = do
    home <- getHomeDirectory
    (configLocation, configPath, autoCreated) <- resolveConfigPath home overridePath
    decoded <- Toml.decodeFileEither rawConfigCodec configPath
    rawConfig <- either (fail . T.unpack . Toml.prettyTomlDecodeErrors) pure decoded
    validated <- validateConfig home configLocation rawConfig
    pure
        ResolvedConfig
            { resolvedPath = configPath
            , resolvedLocation = configLocation
            , resolvedAutoCreated = autoCreated
            , resolvedConfig = validated
            }

resolveConfigPath :: FilePath -> Maybe FilePath -> IO (ConfigLocation, FilePath, Bool)
resolveConfigPath home = \case
    Nothing -> do
        let defaultPath = home </> ".config" </> "tuilauncher" </> "config.toml"
        exists <- doesFileExist defaultPath
        if exists
            then pure (DefaultConfig defaultPath, defaultPath, False)
            else do
                createDirectoryIfMissing True (takeDirectory defaultPath)
                TIO.writeFile defaultPath defaultConfigText
                pure (DefaultConfig defaultPath, defaultPath, True)
    Just path -> do
        expanded <- expandPath home path
        exists <- doesFileExist expanded
        unless exists $
            fail ("Config file does not exist: " <> expanded)
        pure (OverrideConfig expanded, expanded, False)

expandPath :: FilePath -> FilePath -> IO FilePath
expandPath home path
    | "~/" `isPrefixOf` path = pure (home </> drop 2 path)
    | path == "~" = pure home
    | otherwise = pure path

validateConfig :: FilePath -> ConfigLocation -> RawConfig -> IO AppConfig
validateConfig home location RawConfig{..} = do
    layout <- validateLayout rawLayout
    entries <- validateEntries home location rawShell rawEntries
    pure
        AppConfig
            { configTheme = maybe GitHubDark rawThemeName rawTheme
            , configLayout = layout
            , configEntries = entries
            }

validateLayout :: Maybe RawLayout -> IO LayoutConfig
validateLayout maybeLayout = do
    let raw = fromMaybe (RawLayout Nothing Nothing Nothing) maybeLayout
        width = fromMaybe (layoutTileWidth defaultLayout) (rawTileWidth raw)
        height = fromMaybe (layoutTileHeight defaultLayout) (rawTileHeight raw)
        spacing = fromMaybe (layoutTileSpacing defaultLayout) (rawTileSpacing raw)
    when (width <= 0) $ fail "layout.tile-width must be positive"
    when (height <= 0) $ fail "layout.tile-height must be positive"
    when (spacing < 0) $ fail "layout.tile-spacing must be non-negative"
    pure
        LayoutConfig
            { layoutTileWidth = width
            , layoutTileHeight = height
            , layoutTileSpacing = spacing
            }

validateEntries :: FilePath -> ConfigLocation -> Maybe RawShell -> [EntryConfig] -> IO (NonEmpty ResolvedEntry)
validateEntries home location globalShell entries = do
    resolvedEntries <- traverse (resolveEntry home location globalShell) entries
    case resolvedEntries of
        [] -> fail "At least one [[entries]] table is required"
        entry : rest -> pure (entry :| rest)

resolveEntry :: FilePath -> ConfigLocation -> Maybe RawShell -> EntryConfig -> IO ResolvedEntry
resolveEntry home location globalShell EntryConfig{..} = do
    let strippedName = T.strip entryName
        strippedCommand = T.strip entryCommand
    when (T.null strippedName) $ fail "entries.name must not be empty"
    when (T.null strippedCommand) $ fail "entries.command must not be empty"
    workingDir <- traverse (resolveWorkingDir home location) entryWorkingDir
    shellProgramText <- resolveShellProgram home globalShell entryShellProgram
    let login = fromMaybe (maybe False (fromMaybe False . rawShellLogin) globalShell) entryShellLogin
    pure
        ResolvedEntry
            { resolvedName = strippedName
            , resolvedCommand = strippedCommand
            , resolvedWorkingDir = workingDir
            , resolvedShell =
                ShellConfig
                    { shellProgram = T.unpack shellProgramText
                    , shellLogin = login
                    }
            }

resolveShellProgram :: FilePath -> Maybe RawShell -> Maybe T.Text -> IO T.Text
resolveShellProgram _home globalOverride entryOverride =
    case entryOverride <|> (globalOverride >>= rawShellProgram) of
        Just program -> validateShellProgram program
        Nothing -> do
            envShell <- lookupShell
            pure $
                case fmap (T.strip . T.pack) envShell of
                    Just shellPath | not (T.null shellPath) -> shellPath
                    _ -> "/bin/sh"

lookupShell :: IO (Maybe FilePath)
lookupShell = lookupEnv "SHELL"

validateShellProgram :: T.Text -> IO T.Text
validateShellProgram rawProgram = do
    let stripped = T.strip rawProgram
    when (T.null stripped) $
        fail "shell.program and entries.shell-program must not be empty"
    pure stripped

resolveWorkingDir :: FilePath -> ConfigLocation -> T.Text -> IO FilePath
resolveWorkingDir home location rawPath = do
    let strippedPath = T.strip rawPath
    when (T.null strippedPath) $
        fail "entries.working-dir must not be empty"
    let pathString = T.unpack strippedPath
    expanded <- expandPath home pathString
    if isAbsolute expanded
        then pure expanded
        else pure (baseDirectory location home </> expanded)

baseDirectory :: ConfigLocation -> FilePath -> FilePath
baseDirectory location home = case location of
    DefaultConfig _ -> home
    OverrideConfig path -> takeDirectory path

rawConfigCodec :: Toml.TomlCodec RawConfig
rawConfigCodec =
    RawConfig
        <$> (Toml.dioptional (Toml.table rawThemeCodec "theme") Toml..= rawTheme)
        <*> (Toml.dioptional (Toml.table rawLayoutCodec "layout") Toml..= rawLayout)
        <*> (Toml.dioptional (Toml.table rawShellCodec "shell") Toml..= rawShell)
        <*> (Toml.list entryCodec "entries" Toml..= rawEntries)

rawThemeCodec :: Toml.TomlCodec RawTheme
rawThemeCodec =
    RawTheme
        <$> (Toml.textBy renderThemeName parseThemeName "name" Toml..= rawThemeName)

rawLayoutCodec :: Toml.TomlCodec RawLayout
rawLayoutCodec =
    RawLayout
        <$> (Toml.dioptional (Toml.int "tile-width") Toml..= rawTileWidth)
        <*> (Toml.dioptional (Toml.int "tile-height") Toml..= rawTileHeight)
        <*> (Toml.dioptional (Toml.int "tile-spacing") Toml..= rawTileSpacing)

rawShellCodec :: Toml.TomlCodec RawShell
rawShellCodec =
    RawShell
        <$> (Toml.dioptional (Toml.text "program") Toml..= rawShellProgram)
        <*> (Toml.dioptional (Toml.bool "login") Toml..= rawShellLogin)

entryCodec :: Toml.TomlCodec EntryConfig
entryCodec =
    EntryConfig
        <$> (Toml.text "name" Toml..= entryName)
        <*> (Toml.text "command" Toml..= entryCommand)
        <*> (Toml.dioptional (Toml.text "working-dir") Toml..= entryWorkingDir)
        <*> (Toml.dioptional (Toml.text "shell-program") Toml..= entryShellProgram)
        <*> (Toml.dioptional (Toml.bool "shell-login") Toml..= entryShellLogin)
