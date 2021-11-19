module Main
  ( main
  ) where

import           Graphics.WebView

main :: IO ()
main = do
  runWebView False $ do
    title "example"
    size 480 320 ResizeNone
    navigate "https://haskell.org"
