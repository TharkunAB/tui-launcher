#!/usr/bin/env bash

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

export CABAL_DIR="${CABAL_DIR:-$ROOT/.cabal}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$ROOT/.cache}"
FONT_CACHE_DIR="${XDG_CACHE_HOME}/tui-launcher/fonts"
IOSKELEY_RELEASE="2025.10.09-6"
IOSKELEY_ZIP_URL="https://github.com/ahatem/IoskeleyMono/releases/download/${IOSKELEY_RELEASE}/IoskeleyMono-TTF-Hinted.zip"
IOSKELEY_FONT_PATH="${FONT_CACHE_DIR}/IoskeleyMono-Regular.ttf"

mkdir -p "$CABAL_DIR" "$XDG_CACHE_HOME" "$FONT_CACHE_DIR"

if [[ ! -f "$CABAL_DIR/packages/hackage.haskell.org/01-index.tar" && ! -f "$CABAL_DIR/packages/hackage.haskell.org/01-index.tar.gz" ]]; then
  cabal update
fi

cabal build exe:tui-launcher test:tui-launcher-test >/dev/null

LAUNCHER_BIN="${1:-$(cabal list-bin exe:tui-launcher)}"

if [[ ! -x "$LAUNCHER_BIN" ]]; then
  echo "launcher binary not found or not executable: $LAUNCHER_BIN" >&2
  exit 1
fi

ensure_snapshot_font() {
  if [[ -f "$IOSKELEY_FONT_PATH" ]]; then
    return 0
  fi

  local tmpdir
  tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/tui-launcher-font.XXXXXX")"

  curl -fsSL -o "$tmpdir/ioskeley.zip" "$IOSKELEY_ZIP_URL"
  unzip -j -o "$tmpdir/ioskeley.zip" "TTF/IoskeleyMono-Regular.ttf" -d "$FONT_CACHE_DIR" >/dev/null
  rm -rf "$tmpdir"
}

ensure_snapshot_font

TMP_HS="$(mktemp "${TMPDIR:-/tmp}/tui-launcher-snapshots.XXXXXX.hs")"
trap 'rm -f "$TMP_HS"' EXIT

cat >"$TMP_HS" <<'HS'
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import TuiSpec
import TuiSpec.Render (renderAnsiSnapshotFileWithFont)

main :: IO ()
main = do
  [rootDir, launcherPath, fontPath] <- getArgs
  let screenshotsDir = rootDir </> "docs" </> "screenshots"
  createDirectoryIfMissing True screenshotsDir
  forM_ snapshotSpecs $ \(snapshotName, snapshotThemeName) -> do
    let runOptions = runOptionsFor snapshotName snapshotThemeName
    configPath <- writeSnapshotConfig
    withTuiSession runOptions ("snapshot-" <> snapshotName) $ \tui -> do
      launch tui (app launcherPath ["--config", configPath])
      waitForText tui (Nth 0 (Exact "Bash"))
      waitForText tui (Nth 0 (Exact "tmux"))
      waitForText tui (Nth 0 (Exact "Claude"))
      waitForText tui (Nth 0 (Exact "nvim"))
      ansiPath <- dumpView tui (SnapshotName (T.pack snapshotName))
      renderAnsiSnapshotFileWithFont
        (Just fontPath)
        (Just (terminalRows runOptions))
        (Just (terminalCols runOptions))
        (Just snapshotThemeName)
        ansiPath
        (screenshotsDir </> (snapshotName <> ".png"))

runOptionsFor :: FilePath -> String -> RunOptions
runOptionsFor snapshotName snapshotThemeName =
  let (cols, rows) =
        case snapshotName of
          "github-dark" -> (40, 18)
          _ -> (22, 30)
   in defaultRunOptions
        { timeoutSeconds = 15
        , terminalCols = cols
        , terminalRows = rows
        , artifactsDir = "artifacts/theme-snapshots"
        , snapshotTheme = snapshotThemeName
        }

snapshotSpecs :: [(FilePath, String)]
snapshotSpecs =
  [ ("github-light", "pty-default-light")
  , ("github-dark", "pty-default-dark")
  ]

writeSnapshotConfig :: IO FilePath
writeSnapshotConfig = do
  tempDir <- getTemporaryDirectory
  let configDir = tempDir </> "tui-launcher-screenshot"
      configPath = configDir </> "config.toml"
  createDirectoryIfMissing True configDir
  TIO.writeFile configPath snapshotConfig
  pure configPath

snapshotConfig :: T.Text
snapshotConfig =
  T.unlines
    [ "[layout]"
    , "tile-width = 18"
    , "tile-height = 5"
    , "tile-spacing = 1"
    , ""
    , "[[entries]]"
    , "name = \"Bash\""
    , "command = \"exec /bin/bash\""
    , "color = \"green\""
    , "working-dir = \"~\""
    , ""
    , "[[entries]]"
    , "name = \"tmux\""
    , "command = \"tmux\""
    , "color = \"bright-magenta\""
    , "working-dir = \"~/tui-launcher\""
    , ""
    , "[[entries]]"
    , "name = \"Claude\""
    , "command = \"claude\""
    , "color = \"orange\""
    , "working-dir = \"~/tui-launcher\""
    , ""
    , "[[entries]]"
    , "name = \"nvim\""
    , "command = \"nvim\""
    , "color = \"blue\""
    , "working-dir = \"~/tui-launcher\""
    ]
HS

cabal exec runghc "$TMP_HS" -- "$ROOT" "$LAUNCHER_BIN" "$IOSKELEY_FONT_PATH"
