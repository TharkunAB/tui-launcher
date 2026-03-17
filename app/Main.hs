-- | Executable entry point for @tui-launcher@.
module Main (main) where

import TuiLauncher.App qualified as App

-- | Run the application.
main :: IO ()
main = App.main
