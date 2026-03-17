module TUILauncher.Types (
    AppConfig (..),
    AppOptions (..),
    ConfigLocation (..),
    EntryConfig (..),
    GridMetrics (..),
    LayoutConfig (..),
    LaunchSpec (..),
    RawConfig (..),
    RawLayout (..),
    RawShell (..),
    RawTheme (..),
    ResolvedConfig (..),
    ResolvedEntry (..),
    ShellConfig (..),
    ThemeName (..),
    defaultLayout,
    parseThemeName,
    renderThemeName,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

newtype AppOptions = AppOptions
    { appConfigOverride :: Maybe FilePath
    }
    deriving (Eq, Show)

data ConfigLocation
    = DefaultConfig FilePath
    | OverrideConfig FilePath
    deriving (Eq, Show)

data ThemeName
    = GitHubLight
    | GitHubDark
    | GitHubLightHighContrast
    | GitHubDarkHighContrast
    deriving (Bounded, Enum, Eq, Ord, Show)

renderThemeName :: ThemeName -> Text
renderThemeName = \case
    GitHubLight -> "github-light"
    GitHubDark -> "github-dark"
    GitHubLightHighContrast -> "github-light-high-contrast"
    GitHubDarkHighContrast -> "github-dark-high-contrast"

parseThemeName :: Text -> Either Text ThemeName
parseThemeName = \case
    "github-light" -> Right GitHubLight
    "github-dark" -> Right GitHubDark
    "github-light-high-contrast" -> Right GitHubLightHighContrast
    "github-dark-high-contrast" -> Right GitHubDarkHighContrast
    other -> Left $ "Unknown theme: " <> other

newtype RawTheme = RawTheme
    { rawThemeName :: ThemeName
    }
    deriving (Eq, Show)

data RawLayout = RawLayout
    { rawTileWidth :: Maybe Int
    , rawTileHeight :: Maybe Int
    , rawTileSpacing :: Maybe Int
    }
    deriving (Eq, Show)

data RawShell = RawShell
    { rawShellProgram :: Maybe Text
    , rawShellLogin :: Maybe Bool
    }
    deriving (Eq, Show)

data EntryConfig = EntryConfig
    { entryName :: Text
    , entryCommand :: Text
    , entryWorkingDir :: Maybe Text
    , entryShellProgram :: Maybe Text
    , entryShellLogin :: Maybe Bool
    }
    deriving (Eq, Show)

data RawConfig = RawConfig
    { rawTheme :: Maybe RawTheme
    , rawLayout :: Maybe RawLayout
    , rawShell :: Maybe RawShell
    , rawEntries :: [EntryConfig]
    }
    deriving (Eq, Show)

data LayoutConfig = LayoutConfig
    { layoutTileWidth :: Int
    , layoutTileHeight :: Int
    , layoutTileSpacing :: Int
    }
    deriving (Eq, Show)

defaultLayout :: LayoutConfig
defaultLayout =
    LayoutConfig
        { layoutTileWidth = 20
        , layoutTileHeight = 20
        , layoutTileSpacing = 1
        }

data ShellConfig = ShellConfig
    { shellProgram :: FilePath
    , shellLogin :: Bool
    }
    deriving (Eq, Show)

data ResolvedEntry = ResolvedEntry
    { resolvedName :: Text
    , resolvedCommand :: Text
    , resolvedWorkingDir :: Maybe FilePath
    , resolvedShell :: ShellConfig
    }
    deriving (Eq, Show)

data AppConfig = AppConfig
    { configTheme :: ThemeName
    , configLayout :: LayoutConfig
    , configEntries :: NonEmpty ResolvedEntry
    }
    deriving (Eq, Show)

data ResolvedConfig = ResolvedConfig
    { resolvedPath :: FilePath
    , resolvedLocation :: ConfigLocation
    , resolvedAutoCreated :: Bool
    , resolvedConfig :: AppConfig
    }
    deriving (Eq, Show)

data LaunchSpec = LaunchSpec
    { launchCommand :: Text
    , launchWorkingDir :: Maybe FilePath
    , launchShell :: ShellConfig
    }
    deriving (Eq, Show)

data GridMetrics = GridMetrics
    { gridColumns :: Int
    , gridVisibleRows :: Int
    , gridTotalRows :: Int
    }
    deriving (Eq, Show)
