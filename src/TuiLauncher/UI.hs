-- | Brick user interface for browsing and selecting launcher entries.
module TuiLauncher.UI (
    runUi,
) where

import Brick (
    App (..),
    BrickEvent (..),
    EventM,
    Padding (Pad),
    Widget,
    clickable,
    customMain,
    fill,
    hBox,
    hLimit,
    padBottom,
    padRight,
    padTop,
    txt,
    vBox,
    vLimit,
    withAttr,
    (<=>),
 )
import Brick.Main qualified as Brick
import Brick.Types (get, put)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center, hCenter)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import TuiLauncher.Themes
import TuiLauncher.Types

-- | Resource names used by Brick for clickable tiles.
newtype Name = TileName Int deriving (Eq, Ord, Show)

-- | Full UI state tracked during the Brick event loop.
data UiState = UiState
    { uiConfig :: AppConfig
    -- ^ Validated application config.
    , uiSelected :: Int
    -- ^ Index of the currently focused entry.
    , uiScrollRow :: Int
    -- ^ First visible row in the grid.
    , uiWidth :: Int
    -- ^ Last known terminal width.
    , uiHeight :: Int
    -- ^ Last known terminal height.
    , uiLaunch :: Maybe LaunchSpec
    -- ^ Launch request chosen by the user, if any.
    , uiStatus :: Maybe Text
    -- ^ Optional footer override message.
    }

-- | Run the Brick UI and return the selected launch request, if any.
runUi :: AppConfig -> IO (Maybe LaunchSpec)
runUi config = do
    initialVty <- mkVty defaultConfig
    (w, h) <- V.displayBounds (V.outputIface initialVty)
    finalState <-
        customMain
            initialVty
            (mkVty defaultConfig)
            Nothing
            app
            UiState
                { uiConfig = config
                , uiSelected = 0
                , uiScrollRow = 0
                , uiWidth = w
                , uiHeight = h
                , uiLaunch = Nothing
                , uiStatus = Nothing
                }
    pure (uiLaunch finalState)

-- | Brick application definition.
app :: App UiState e Name
app =
    App
        { appDraw = drawUi
        , appChooseCursor = Brick.neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = pure ()
        , appAttrMap = attrMapForTheme . configTheme . uiConfig
        }

-- | Draw the complete screen.
drawUi :: UiState -> [Widget Name]
drawUi state =
    [ withAttr baseAttr $
        header state
            <=> padTop (Pad 1) (drawBody state)
            <=> padTop (Pad 1) (footer state)
    ]

-- | Render the application title.
header :: UiState -> Widget Name
header _state =
    withAttr accentAttr $
        hCenter $
            txt "tui-launcher"

-- | Render the footer help text or status line.
footer :: UiState -> Widget Name
footer state =
    let info =
            fromMaybe
                "Arrows/hjkl move, Enter launches, click launches, q/Esc quits"
                (uiStatus state)
     in withAttr mutedAttr (txt info)

-- | Render the grid body or the small-window fallback.
drawBody :: UiState -> Widget Name
drawBody state
    | tooSmall = center $ withAttr mutedAttr $ txt "Window too small"
    | otherwise = vBox visibleRowsWidgets
  where
    layout = configLayout (uiConfig state)
    metrics = gridMetrics layout (uiWidth state) (uiHeight state) (entryCount state)
    tooSmall =
        layoutTileWidth layout + 2 > uiWidth state
            || layoutTileHeight layout + 3 > uiHeight state
    entries = zip [0 ..] (NonEmpty.toList (configEntries (uiConfig state)))
    rows = chunk (gridColumns metrics) entries
    visibleRows = take (gridVisibleRows metrics) (drop (uiScrollRow state) rows)
    visibleRowsWidgets = zipWith (drawRow state) [uiScrollRow state ..] visibleRows

-- | Render one visible row of tiles.
drawRow :: UiState -> Int -> [(Int, ResolvedEntry)] -> Widget Name
drawRow state _rowIndex entriesInRow =
    padBottom (Pad (layoutTileSpacing (configLayout (uiConfig state)))) $
        hBox $
            zipWith
                ( \position tileWidget ->
                    if position == length entriesInRow - 1
                        then tileWidget
                        else padRight (Pad (layoutTileSpacing (configLayout (uiConfig state)))) tileWidget
                )
                [0 ..]
                (fmap (uncurry (drawTile state)) entriesInRow)

-- | Render a single selectable tile.
drawTile :: UiState -> Int -> ResolvedEntry -> Widget Name
drawTile state index entry =
    clickable (TileName index) $
        hLimit (layoutTileWidth layout) $
            vLimit (layoutTileHeight layout) $
                withAttr tileAttr $
                    border $
                        vBox
                            [ vLimit topPad (fill ' ')
                            , center $ txt (resolvedName entry)
                            , center $
                                withAttr mutedAttr $
                                    txt $
                                        truncateText innerWidth (resolvedCommand entry)
                            , fill ' '
                            ]
  where
    layout = configLayout (uiConfig state)
    innerWidth = max 1 (layoutTileWidth layout - 2)
    innerHeight = max 1 (layoutTileHeight layout - 2)
    topPad = max 0 ((innerHeight - 2) `div` 2)
    tileAttr = if index == uiSelected state then focusedAttr else borderAttr

-- | Handle keyboard, mouse, and resize events.
handleEvent :: BrickEvent Name e -> EventM Name UiState ()
handleEvent event = do
    state <- get
    case event of
        VtyEvent (V.EvResize width height) ->
            put $ normalizeScroll state{uiWidth = width, uiHeight = height}
        VtyEvent (V.EvKey V.KEnter []) ->
            put (launchSelected state) >> Brick.halt
        VtyEvent (V.EvKey V.KEsc []) ->
            Brick.halt
        VtyEvent (V.EvKey (V.KChar 'q') []) ->
            Brick.halt
        VtyEvent (V.EvKey V.KLeft []) ->
            put $ moveHorizontal (-1) state
        VtyEvent (V.EvKey (V.KChar 'h') []) ->
            put $ moveHorizontal (-1) state
        VtyEvent (V.EvKey V.KRight []) ->
            put $ moveHorizontal 1 state
        VtyEvent (V.EvKey (V.KChar 'l') []) ->
            put $ moveHorizontal 1 state
        VtyEvent (V.EvKey V.KUp []) ->
            put $ moveVertical (-1) state
        VtyEvent (V.EvKey (V.KChar 'k') []) ->
            put $ moveVertical (-1) state
        VtyEvent (V.EvKey V.KDown []) ->
            put $ moveVertical 1 state
        VtyEvent (V.EvKey (V.KChar 'j') []) ->
            put $ moveVertical 1 state
        MouseDown (TileName index) V.BLeft [] _ ->
            put (launchByIndex index state) >> Brick.halt
        MouseDown _ V.BScrollDown [] _ ->
            put $ scrollRows 1 state
        MouseDown _ V.BScrollUp [] _ ->
            put $ scrollRows (-1) state
        _ -> pure ()

-- | Launch the currently selected entry.
launchSelected :: UiState -> UiState
launchSelected state = launchByIndex (uiSelected state) state

-- | Convert an entry index into the corresponding launch request.
launchByIndex :: Int -> UiState -> UiState
launchByIndex index state =
    state
        { uiSelected = boundedIndex
        , uiLaunch =
            Just
                LaunchSpec
                    { launchCommand = resolvedCommand entry
                    , launchWorkingDir = resolvedWorkingDir entry
                    , launchShell = resolvedShell entry
                    }
        }
  where
    entries = NonEmpty.toList (configEntries (uiConfig state))
    boundedIndex = clamp 0 (length entries - 1) index
    entry = entries !! boundedIndex

-- | Move the focused tile left or right.
moveHorizontal :: Int -> UiState -> UiState
moveHorizontal delta state =
    normalizeScroll state{uiSelected = clamp 0 (entryCount state - 1) (uiSelected state + delta)}

-- | Move the focused tile up or down by one grid row.
moveVertical :: Int -> UiState -> UiState
moveVertical delta state =
    normalizeScroll state{uiSelected = clamp 0 (entryCount state - 1) (uiSelected state + delta * gridColumns metrics)}
  where
    metrics = gridMetrics (configLayout (uiConfig state)) (uiWidth state) (uiHeight state) (entryCount state)

-- | Scroll the viewport by a number of rows.
scrollRows :: Int -> UiState -> UiState
scrollRows delta state =
    state{uiScrollRow = clamp 0 maxScroll (uiScrollRow state + delta)}
  where
    metrics = gridMetrics (configLayout (uiConfig state)) (uiWidth state) (uiHeight state) (entryCount state)
    maxScroll = max 0 (gridTotalRows metrics - gridVisibleRows metrics)

-- | Keep the selected tile visible after movement or resize events.
normalizeScroll :: UiState -> UiState
normalizeScroll state =
    state{uiScrollRow = clamp 0 maxScroll targetScroll}
  where
    metrics = gridMetrics (configLayout (uiConfig state)) (uiWidth state) (uiHeight state) (entryCount state)
    maxScroll = max 0 (gridTotalRows metrics - gridVisibleRows metrics)
    selectedRow = uiSelected state `div` gridColumns metrics
    targetScroll
        | selectedRow < uiScrollRow state = selectedRow
        | selectedRow >= uiScrollRow state + gridVisibleRows metrics =
            selectedRow - gridVisibleRows metrics + 1
        | otherwise = uiScrollRow state

-- | Compute grid dimensions for the current terminal size and layout.
gridMetrics :: LayoutConfig -> Int -> Int -> Int -> GridMetrics
gridMetrics layout width height totalEntries =
    GridMetrics
        { gridColumns = max 1 columns
        , gridVisibleRows = max 1 visibleRows
        , gridTotalRows = max 1 totalRows
        }
  where
    outerWidth = layoutTileWidth layout + layoutTileSpacing layout
    outerHeight = layoutTileHeight layout + layoutTileSpacing layout
    bodyHeight = max 1 (height - 4)
    columns = max 1 ((width + layoutTileSpacing layout) `div` max 1 outerWidth)
    visibleRows = max 1 ((bodyHeight + layoutTileSpacing layout) `div` max 1 outerHeight)
    totalRows = ceilingDiv totalEntries columns

-- | Count the configured entries.
entryCount :: UiState -> Int
entryCount = length . NonEmpty.toList . configEntries . uiConfig

-- | Divide and round up, returning a safe minimum of one.
ceilingDiv :: Int -> Int -> Int
ceilingDiv numerator denominator
    | denominator <= 0 = 1
    | numerator <= 0 = 1
    | otherwise = (numerator + denominator - 1) `div` denominator

-- | Split a list into equally sized chunks.
chunk :: Int -> [a] -> [[a]]
chunk size xs
    | size <= 0 = [xs]
    | null xs = []
    | otherwise =
        let (prefix, rest) = splitAt size xs
         in prefix : chunk size rest

-- | Truncate text to fit a tile, appending an ellipsis when needed.
truncateText :: Int -> Text -> Text
truncateText width textValue
    | width <= 0 = ""
    | T.length textValue <= width = textValue
    | width == 1 = "…"
    | otherwise = T.take (width - 1) textValue <> "…"

-- | Clamp a value to an inclusive range.
clamp :: Int -> Int -> Int -> Int
clamp lower upper value = max lower (min upper value)
