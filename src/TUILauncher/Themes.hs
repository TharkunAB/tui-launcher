module TUILauncher.Themes (
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
import TUILauncher.Types (ThemeName (..))

baseAttr, borderAttr, mutedAttr, accentAttr, focusedAttr :: AttrName
baseAttr = attrName "base"
borderAttr = attrName "border"
mutedAttr = attrName "muted"
accentAttr = attrName "accent"
focusedAttr = attrName "focused"

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

baseStyle :: ThemeName -> V.Attr
baseStyle = \case
    GitHubLight -> makeAttr V.black V.white
    GitHubDark -> makeAttr V.white V.black
    GitHubLightHighContrast -> makeAttr V.black V.brightWhite
    GitHubDarkHighContrast -> makeAttr V.brightWhite V.black

borderStyle :: ThemeName -> V.Attr
borderStyle = \case
    GitHubLight -> makeAttr V.brightBlack V.white
    GitHubDark -> makeAttr V.brightBlack V.black
    GitHubLightHighContrast -> makeAttr V.blue V.brightWhite
    GitHubDarkHighContrast -> makeAttr V.brightCyan V.black

mutedStyle :: ThemeName -> V.Attr
mutedStyle = \case
    GitHubLight -> makeAttr V.brightBlack V.white
    GitHubDark -> makeAttr V.brightBlack V.black
    GitHubLightHighContrast -> makeAttr V.blue V.brightWhite
    GitHubDarkHighContrast -> makeAttr V.cyan V.black

accentStyle :: ThemeName -> V.Attr
accentStyle = \case
    GitHubLight -> makeAttr V.blue V.white
    GitHubDark -> makeAttr V.brightBlue V.black
    GitHubLightHighContrast -> makeAttr V.blue V.brightWhite
    GitHubDarkHighContrast -> makeAttr V.brightCyan V.black

focusedStyle :: ThemeName -> V.Attr
focusedStyle = \case
    GitHubLight -> V.withStyle (V.white `on` V.blue) V.bold
    GitHubDark -> V.withStyle (V.black `on` V.brightBlue) V.bold
    GitHubLightHighContrast -> V.withStyle (V.white `on` V.blue) V.bold
    GitHubDarkHighContrast -> V.withStyle (V.black `on` V.brightCyan) V.bold

makeAttr :: V.Color -> V.Color -> V.Attr
makeAttr fore back = V.defAttr `V.withForeColor` fore `V.withBackColor` back
