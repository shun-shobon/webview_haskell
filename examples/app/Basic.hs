module Main where

import           Graphics.WebView

main :: IO ()
main = do
  runWebView True $ do
    title "example"
    size 480 320 ResizeNone
    navigate "https://haskell.org"
    bind "print" $ \s -> do
      putStrLn s
      pure . Right $ "\"print done.\""
