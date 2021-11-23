{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader           ( ask
                                                , liftIO
                                                , runReaderT
                                                )
import qualified Data.Text                     as T
import           Graphics.WebView

main :: IO ()
main = do
  runWebView True $ do
    w <- ask
    title "example"
    size (Size 480 320) True
    navigate "https://haskell.org"
    initJS "console.log('Hello, WebView!')"
    bind "print" $ \s -> do
      runReaderT (evalJS "console.log('printed!')") w
      putStrLn . T.unpack $ s
      pure . Right $ "\"print done.\""
