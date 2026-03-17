-- | Shared configuration and UI domain types.
module TuiLauncher.Types (
    AppConfig (..),
    AppOptions (..),
    ConfigLocation (..),
    EntryColor (..),
    EntryConfig (..),
    GridMetrics (..),
    LayoutConfig (..),
    LaunchSpec (..),
    RawConfig (..),
    RawLayout (..),
    RawShell (..),
    ResolvedConfig (..),
    ResolvedEntry (..),
    ShellConfig (..),
    defaultLayout,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

-- | Command-line options accepted by the executable.
newtype AppOptions = AppOptions
    { appConfigOverride :: Maybe FilePath
    -- ^ Optional explicit config path from @--config@.
    }
    deriving (Eq, Show)

-- | Where the active configuration file was loaded from.
data ConfigLocation
    = -- | The default config under @~/.config/tui-launcher/config.toml@.
      DefaultConfig FilePath
    | -- | A path supplied explicitly via @--config@.
      OverrideConfig FilePath
    deriving (Eq, Show)

-- | Supported ANSI text colors for entry labels and commands.
data EntryColor
    = EntryBlack
    | EntryRed
    | EntryOrange
    | EntryGreen
    | EntryYellow
    | EntryBlue
    | EntryMagenta
    | EntryCyan
    | EntryWhite
    | EntryBrightBlack
    | EntryBrightRed
    | EntryBrightGreen
    | EntryBrightYellow
    | EntryBrightBlue
    | EntryBrightMagenta
    | EntryBrightCyan
    | EntryBrightWhite
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | Raw layout settings parsed directly from TOML.
data RawLayout = RawLayout
    { rawTileWidth :: Maybe Int
    -- ^ Optional tile width override.
    , rawTileHeight :: Maybe Int
    -- ^ Optional tile height override.
    , rawTileSpacing :: Maybe Int
    -- ^ Optional tile spacing override.
    }
    deriving (Eq, Show)

-- | Raw shell defaults parsed directly from TOML.
data RawShell = RawShell
    { rawShellProgram :: Maybe Text
    -- ^ Optional shell executable.
    , rawShellLogin :: Maybe Bool
    -- ^ Optional login-shell flag.
    }
    deriving (Eq, Show)

-- | Raw entry table parsed directly from TOML.
data EntryConfig = EntryConfig
    { entryName :: Text
    -- ^ Label shown in the UI.
    , entryCommand :: Text
    -- ^ Command passed to the selected shell.
    , entryColor :: Maybe Text
    -- ^ Optional text color for this entry.
    , entryWorkingDir :: Maybe Text
    -- ^ Optional working directory before launch.
    , entryShellProgram :: Maybe Text
    -- ^ Optional per-entry shell override.
    , entryShellLogin :: Maybe Bool
    -- ^ Optional per-entry login-shell flag.
    }
    deriving (Eq, Show)

-- | Top-level TOML structure before validation and normalization.
data RawConfig = RawConfig
    { rawLayout :: Maybe RawLayout
    -- ^ Optional layout table.
    , rawShell :: Maybe RawShell
    -- ^ Optional global shell defaults.
    , rawEntries :: [EntryConfig]
    -- ^ Launcher entries in display order.
    }
    deriving (Eq, Show)

-- | Validated layout settings used by the UI.
data LayoutConfig = LayoutConfig
    { layoutTileWidth :: Int
    -- ^ Width of each tile in terminal cells.
    , layoutTileHeight :: Int
    -- ^ Height of each tile in terminal cells.
    , layoutTileSpacing :: Int
    -- ^ Horizontal and vertical spacing between tiles.
    }
    deriving (Eq, Show)

-- | Built-in default layout used when the config omits layout values.
defaultLayout :: LayoutConfig
defaultLayout =
    LayoutConfig
        { layoutTileWidth = 20
        , layoutTileHeight = 20
        , layoutTileSpacing = 1
        }

-- | Shell program and flags used to execute a launch command.
data ShellConfig = ShellConfig
    { shellProgram :: FilePath
    -- ^ Executable name or path for the shell.
    , shellLogin :: Bool
    -- ^ Whether the shell should be invoked as a login shell.
    }
    deriving (Eq, Show)

-- | A fully validated entry ready for display in the UI.
data ResolvedEntry = ResolvedEntry
    { resolvedName :: Text
    -- ^ Label rendered in the tile.
    , resolvedCommand :: Text
    -- ^ Command string passed to the shell.
    , resolvedColor :: Maybe EntryColor
    -- ^ Optional terminal color used for the tile text.
    , resolvedWorkingDirDisplay :: Maybe Text
    -- ^ Optional working directory label rendered in the tile.
    , resolvedWorkingDir :: Maybe FilePath
    -- ^ Optional working directory applied before launch.
    , resolvedShell :: ShellConfig
    -- ^ Resolved shell configuration for this entry.
    }
    deriving (Eq, Show)

-- | Validated application configuration consumed by the UI.
data AppConfig = AppConfig
    { configLayout :: LayoutConfig
    -- ^ Layout parameters for the tile grid.
    , configEntries :: NonEmpty ResolvedEntry
    -- ^ Entries shown in the launcher.
    }
    deriving (Eq, Show)

-- | Full result of loading and validating the configuration file.
data ResolvedConfig = ResolvedConfig
    { resolvedPath :: FilePath
    -- ^ Final path used to load the config.
    , resolvedLocation :: ConfigLocation
    -- ^ Whether the path was default or overridden.
    , resolvedAutoCreated :: Bool
    -- ^ Whether the default config had to be created on startup.
    , resolvedConfig :: AppConfig
    -- ^ Validated application configuration.
    }
    deriving (Eq, Show)

-- | Launch request returned by the UI when the user selects an entry.
data LaunchSpec = LaunchSpec
    { launchCommand :: Text
    -- ^ Command string passed to the shell.
    , launchWorkingDir :: Maybe FilePath
    -- ^ Optional working directory to switch to before launch.
    , launchShell :: ShellConfig
    -- ^ Shell configuration used to execute the command.
    }
    deriving (Eq, Show)

-- | Derived grid sizing information for a particular terminal size.
data GridMetrics = GridMetrics
    { gridColumns :: Int
    -- ^ Number of tile columns that fit horizontally.
    , gridVisibleRows :: Int
    -- ^ Number of tile rows visible without scrolling.
    , gridTotalRows :: Int
    -- ^ Total number of tile rows in the entry list.
    }
    deriving (Eq, Show)
