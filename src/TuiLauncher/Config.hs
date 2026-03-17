{-# LANGUAGE MultilineStrings #-}

-- | Config file loading, validation, and normalization.
module TuiLauncher.Config (
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
import Toml qualified
import TuiLauncher.Types

{- | Starter config written on first launch when no default config exists.

The generated file keeps only the shell entry enabled and leaves the other
starter entries in place as commented examples.
-}
defaultConfigText :: T.Text
defaultConfigText =
    """
    [layout]
    tile-width = 20
    tile-height = 5
    tile-spacing = 1

    # [shell]
    # program = "/bin/bash"
    # login = false

    [[entries]]
    name = "Shell"
    command = "exec \\\"${SHELL:-/bin/sh}\\\""
    # color = "bright-blue"

    # [[entries]]
    # name = "nvim"
    # command = "nvim"
    # color = "blue"
    # working-dir = "~/Code"

    # [[entries]]
    # name = "Tmux"
    # command = "tmux"
    # color = "cyan"
    # working-dir = "~/Code/project"
    # shell-program = "/bin/zsh"
    # shell-login = true

    # [[entries]]
    # name = "Codex"
    # command = "codex"
    # color = "bright-magenta"

    # [[entries]]
    # name = "Claude"
    # command = "claude"
    # color = "orange"
    """

-- | Load, parse, validate, and normalize the active config file.
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

-- | Resolve which config file to use and create the default one if needed.
resolveConfigPath :: FilePath -> Maybe FilePath -> IO (ConfigLocation, FilePath, Bool)
resolveConfigPath home = \case
    Nothing -> do
        let defaultPath = home </> ".config" </> "tui-launcher" </> "config.toml"
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

-- | Expand a leading @~@ against the supplied home directory.
expandPath :: FilePath -> FilePath -> IO FilePath
expandPath home path
    | "~/" `isPrefixOf` path = pure (home </> drop 2 path)
    | path == "~" = pure home
    | otherwise = pure path

-- | Validate raw TOML data into the application config used by the UI.
validateConfig :: FilePath -> ConfigLocation -> RawConfig -> IO AppConfig
validateConfig home location RawConfig{..} = do
    layout <- validateLayout rawLayout
    entries <- validateEntries home location rawShell rawEntries
    pure
        AppConfig
            { configLayout = layout
            , configEntries = entries
            }

-- | Validate layout values and apply defaults for missing settings.
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

-- | Validate and resolve all configured entries.
validateEntries :: FilePath -> ConfigLocation -> Maybe RawShell -> [EntryConfig] -> IO (NonEmpty ResolvedEntry)
validateEntries home location globalShell entries = do
    resolvedEntries <- traverse (resolveEntry home location globalShell) entries
    case resolvedEntries of
        [] -> fail "At least one [[entries]] table is required"
        entry : rest -> pure (entry :| rest)

-- | Resolve a single entry into the runtime format used by the launcher.
resolveEntry :: FilePath -> ConfigLocation -> Maybe RawShell -> EntryConfig -> IO ResolvedEntry
resolveEntry home location globalShell EntryConfig{..} = do
    let strippedName = T.strip entryName
        strippedCommand = T.strip entryCommand
    when (T.null strippedName) $ fail "entries.name must not be empty"
    when (T.null strippedCommand) $ fail "entries.command must not be empty"
    color <- traverse resolveEntryColor entryColor
    workingDir <- traverse (resolveWorkingDir home location) entryWorkingDir
    shellProgramText <- resolveShellProgram home globalShell entryShellProgram
    let login = fromMaybe (maybe False (fromMaybe False . rawShellLogin) globalShell) entryShellLogin
    pure
        ResolvedEntry
            { resolvedName = strippedName
            , resolvedCommand = strippedCommand
            , resolvedColor = color
            , resolvedWorkingDirDisplay = fmap (displayWorkingDir home) workingDir
            , resolvedWorkingDir = workingDir
            , resolvedShell =
                ShellConfig
                    { shellProgram = T.unpack shellProgramText
                    , shellLogin = login
                    }
            }

-- | Resolve the shell program for an entry using the configured precedence.
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

-- | Read the current shell from the environment.
lookupShell :: IO (Maybe FilePath)
lookupShell = lookupEnv "SHELL"

-- | Reject blank shell program values.
validateShellProgram :: T.Text -> IO T.Text
validateShellProgram rawProgram = do
    let stripped = T.strip rawProgram
    when (T.null stripped) $
        fail "shell.program and entries.shell-program must not be empty"
    pure stripped

-- | Resolve an entry working directory relative to the proper base path.
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

-- | Determine the base directory for relative path resolution.
baseDirectory :: ConfigLocation -> FilePath -> FilePath
baseDirectory location home = case location of
    DefaultConfig _ -> home
    OverrideConfig path -> takeDirectory path

-- | Render a resolved working directory with @~@ for the user's home.
displayWorkingDir :: FilePath -> FilePath -> T.Text
displayWorkingDir home path
    | path == home = "~"
    | (home <> "/") `isPrefixOf` path =
        T.pack ("~/" <> drop (length home + 1) path)
    | otherwise = T.pack path

-- | Validate and normalize an optional entry color.
resolveEntryColor :: T.Text -> IO EntryColor
resolveEntryColor rawColor = do
    let normalized = T.toLower (T.strip rawColor)
    when (T.null normalized) $
        fail "entries.color must not be empty"
    case normalized of
        "black" -> pure EntryBlack
        "red" -> pure EntryRed
        "orange" -> pure EntryOrange
        "green" -> pure EntryGreen
        "yellow" -> pure EntryYellow
        "blue" -> pure EntryBlue
        "magenta" -> pure EntryMagenta
        "cyan" -> pure EntryCyan
        "white" -> pure EntryWhite
        "bright-black" -> pure EntryBrightBlack
        "bright-red" -> pure EntryBrightRed
        "bright-green" -> pure EntryBrightGreen
        "bright-yellow" -> pure EntryBrightYellow
        "bright-blue" -> pure EntryBrightBlue
        "bright-magenta" -> pure EntryBrightMagenta
        "bright-cyan" -> pure EntryBrightCyan
        "bright-white" -> pure EntryBrightWhite
        "gray" -> pure EntryBrightBlack
        "grey" -> pure EntryBrightBlack
        other ->
            fail
                ( "Unsupported entries.color: "
                    <> T.unpack other
                    <> ". Use an ANSI color name like red, bright-blue, or cyan."
                )

-- | TOML codec for the top-level config structure.
rawConfigCodec :: Toml.TomlCodec RawConfig
rawConfigCodec =
    RawConfig
        <$> (Toml.dioptional (Toml.table rawLayoutCodec "layout") Toml..= rawLayout)
        <*> (Toml.dioptional (Toml.table rawShellCodec "shell") Toml..= rawShell)
        <*> (Toml.list entryCodec "entries" Toml..= rawEntries)

-- | TOML codec for the layout table.
rawLayoutCodec :: Toml.TomlCodec RawLayout
rawLayoutCodec =
    RawLayout
        <$> (Toml.dioptional (Toml.int "tile-width") Toml..= rawTileWidth)
        <*> (Toml.dioptional (Toml.int "tile-height") Toml..= rawTileHeight)
        <*> (Toml.dioptional (Toml.int "tile-spacing") Toml..= rawTileSpacing)

-- | TOML codec for global shell defaults.
rawShellCodec :: Toml.TomlCodec RawShell
rawShellCodec =
    RawShell
        <$> (Toml.dioptional (Toml.text "program") Toml..= rawShellProgram)
        <*> (Toml.dioptional (Toml.bool "login") Toml..= rawShellLogin)

-- | TOML codec for launcher entries.
entryCodec :: Toml.TomlCodec EntryConfig
entryCodec =
    EntryConfig
        <$> (Toml.text "name" Toml..= entryName)
        <*> (Toml.text "command" Toml..= entryCommand)
        <*> (Toml.dioptional (Toml.text "color") Toml..= entryColor)
        <*> (Toml.dioptional (Toml.text "working-dir") Toml..= entryWorkingDir)
        <*> (Toml.dioptional (Toml.text "shell-program") Toml..= entryShellProgram)
        <*> (Toml.dioptional (Toml.bool "shell-login") Toml..= entryShellLogin)
