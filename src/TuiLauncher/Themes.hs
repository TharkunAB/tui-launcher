-- | Brick attribute definitions that defer colors to the active terminal theme.
module TuiLauncher.Themes (
    accentAttr,
    attrMapForTerminal,
    baseAttr,
    borderAttr,
    entryCommandAttr,
    exitAttr,
    exitFocusedAttr,
    focusedAttr,
    focusedTitleAttr,
    mutedAttr,
) where

import Brick (AttrMap, AttrName, attrMap, attrName)
import Graphics.Vty qualified as V
import TuiLauncher.Types (AppConfig (..), EntryColor (..), ResolvedEntry (..))

-- | Default text and background styling.
baseAttr :: AttrName
baseAttr = attrName "base"

-- | Border styling for tiles and layout chrome.
borderAttr :: AttrName
borderAttr = attrName "border"

-- | Secondary text styling for commands.
mutedAttr :: AttrName
mutedAttr = attrName "muted"

-- | Accent styling for headings.
accentAttr :: AttrName
accentAttr = attrName "accent"

-- | Styling for the currently focused tile.
focusedAttr :: AttrName
focusedAttr = attrName "focused"

-- | Styling for the title inside the currently focused tile.
focusedTitleAttr :: AttrName
focusedTitleAttr = attrName "focused.title"

-- | Styling for the exit button when it is not selected.
exitAttr :: AttrName
exitAttr = attrName "exit"

-- | Styling for the exit button when it is selected.
exitFocusedAttr :: AttrName
exitFocusedAttr = attrName "exit.focused"

-- | Attribute name for the colored command text inside an entry tile.
entryCommandAttr :: Maybe EntryColor -> AttrName
entryCommandAttr maybeColor =
    attrName ("entry.command." <> maybe "default" renderEntryColor maybeColor)

-- | Build the Brick attribute map using terminal-default colors.
attrMapForTerminal :: AppConfig -> AttrMap
attrMapForTerminal config =
    attrMap
        V.defAttr
        ( [ (baseAttr, V.defAttr)
          , (borderAttr, V.withForeColor V.defAttr V.brightBlack)
          , (mutedAttr, V.withStyle V.defAttr V.dim)
          , (accentAttr, V.withStyle V.defAttr V.bold)
          , (focusedAttr, V.withStyle V.defAttr V.bold)
          , (focusedTitleAttr, V.withStyle V.defAttr V.bold)
          , (exitAttr, V.withForeColor V.defAttr V.brightBlack)
          , (exitFocusedAttr, V.withStyle V.defAttr V.bold)
          ]
            <> concatMap entryColorAttrs (configEntries config)
        )

-- | Build text attrs for an entry's configured color.
entryColorAttrs :: ResolvedEntry -> [(AttrName, V.Attr)]
entryColorAttrs entry =
    case resolvedColor entry of
        Nothing -> [(entryCommandAttr Nothing, V.withStyle V.defAttr V.dim)]
        Just colorValue ->
            let color = entryColorToVty colorValue
             in [(entryCommandAttr (Just colorValue), V.withForeColor V.defAttr color)]

-- | Render an entry color to a stable attribute suffix.
renderEntryColor :: EntryColor -> String
renderEntryColor = \case
    EntryBlack -> "black"
    EntryRed -> "red"
    EntryOrange -> "orange"
    EntryGreen -> "green"
    EntryYellow -> "yellow"
    EntryBlue -> "blue"
    EntryMagenta -> "magenta"
    EntryCyan -> "cyan"
    EntryWhite -> "white"
    EntryBrightBlack -> "bright-black"
    EntryBrightRed -> "bright-red"
    EntryBrightGreen -> "bright-green"
    EntryBrightYellow -> "bright-yellow"
    EntryBrightBlue -> "bright-blue"
    EntryBrightMagenta -> "bright-magenta"
    EntryBrightCyan -> "bright-cyan"
    EntryBrightWhite -> "bright-white"

-- | Map an entry color to the corresponding Vty color.
entryColorToVty :: EntryColor -> V.Color
entryColorToVty = \case
    EntryBlack -> V.black
    EntryRed -> V.red
    EntryOrange -> V.rgbColor (255 :: Int) 165 0
    EntryGreen -> V.green
    EntryYellow -> V.yellow
    EntryBlue -> V.blue
    EntryMagenta -> V.magenta
    EntryCyan -> V.cyan
    EntryWhite -> V.white
    EntryBrightBlack -> V.brightBlack
    EntryBrightRed -> V.brightRed
    EntryBrightGreen -> V.brightGreen
    EntryBrightYellow -> V.brightYellow
    EntryBrightBlue -> V.brightBlue
    EntryBrightMagenta -> V.brightMagenta
    EntryBrightCyan -> V.brightCyan
    EntryBrightWhite -> V.brightWhite
