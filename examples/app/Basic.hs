module Main where

import           Control.Monad.Reader           ( ask
                                                , liftIO
                                                , runReaderT
                                                )
import           Graphics.WebView

main :: IO ()
main = do
  runWebView True $ do
    w <- ask
    title "example"
    size 480 320 ResizeNone
    navigate "https://haskell.org"
    initJS "console.log('Hello, WebView!')"
    bind "print" $ \s -> do
      runReaderT (evalJS "console.log('printed!')") w
      putStrLn s
      pure . Right $ "\"print done.\""
