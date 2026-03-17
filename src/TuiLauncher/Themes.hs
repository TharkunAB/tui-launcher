-- | Brick attribute definitions for the built-in themes.
module TuiLauncher.Themes (
    accentAttr,
    attrMapForTheme,
    baseAttr,
    borderAttr,
    focusedAttr,
    mutedAttr,
) where

import Brick (AttrMap, AttrName, attrMap, attrName)
import Brick.Util (on)
import Graphics.Vty qualified as V
import TuiLauncher.Types (ThemeName (..))

-- | Default text and background styling.
baseAttr :: AttrName
baseAttr = attrName "base"

-- | Border styling for tiles and layout chrome.
borderAttr :: AttrName
borderAttr = attrName "border"

-- | Secondary text styling for hints and commands.
mutedAttr :: AttrName
mutedAttr = attrName "muted"

-- | Accent styling for headings and highlights.
accentAttr :: AttrName
accentAttr = attrName "accent"

-- | Styling for the currently focused tile.
focusedAttr :: AttrName
focusedAttr = attrName "focused"

-- | Build the Brick attribute map for a configured theme.
attrMapForTheme :: ThemeName -> AttrMap
attrMapForTheme themeName =
    attrMap
        (baseStyle themeName)
        [ (baseAttr, baseStyle themeName)
        , (borderAttr, borderStyle themeName)
        , (mutedAttr, mutedStyle themeName)
        , (accentAttr, accentStyle themeName)
        , (focusedAttr, focusedStyle themeName)
        ]

-- | Base foreground and background colors for a theme.
baseStyle :: ThemeName -> V.Attr
baseStyle = \case
    GitHubLight -> makeAttr V.black V.white
    GitHubDark -> makeAttr V.white V.black
    GitHubLightHighContrast -> makeAttr V.black V.brightWhite
    GitHubDarkHighContrast -> makeAttr V.brightWhite V.black

-- | Border color for a theme.
borderStyle :: ThemeName -> V.Attr
borderStyle = \case
    GitHubLight -> makeAttr V.brightBlack V.white
    GitHubDark -> makeAttr V.brightBlack V.black
    GitHubLightHighContrast -> makeAttr V.blue V.brightWhite
    GitHubDarkHighContrast -> makeAttr V.brightCyan V.black

-- | Muted text color for a theme.
mutedStyle :: ThemeName -> V.Attr
mutedStyle = \case
    GitHubLight -> makeAttr V.brightBlack V.white
    GitHubDark -> makeAttr V.brightBlack V.black
    GitHubLightHighContrast -> makeAttr V.blue V.brightWhite
    GitHubDarkHighContrast -> makeAttr V.cyan V.black

-- | Accent color for headings and focus hints.
accentStyle :: ThemeName -> V.Attr
accentStyle = \case
    GitHubLight -> makeAttr V.blue V.white
    GitHubDark -> makeAttr V.brightBlue V.black
    GitHubLightHighContrast -> makeAttr V.blue V.brightWhite
    GitHubDarkHighContrast -> makeAttr V.brightCyan V.black

-- | Focused tile style for a theme.
focusedStyle :: ThemeName -> V.Attr
focusedStyle = \case
    GitHubLight -> V.withStyle (V.white `on` V.blue) V.bold
    GitHubDark -> V.withStyle (V.black `on` V.brightBlue) V.bold
    GitHubLightHighContrast -> V.withStyle (V.white `on` V.blue) V.bold
    GitHubDarkHighContrast -> V.withStyle (V.black `on` V.brightCyan) V.bold

-- | Construct a simple foreground/background attribute pair.
makeAttr :: V.Color -> V.Color -> V.Attr
makeAttr fore back = V.defAttr `V.withForeColor` fore `V.withBackColor` back
