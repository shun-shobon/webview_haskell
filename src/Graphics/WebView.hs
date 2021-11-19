module Graphics.WebView
  ( WebView
  , WebViewM
  , runWebView
  , title
  , size
  , navigate
  , ResizeHint(..)
  ) where

import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , liftIO
                                                , runReaderT
                                                )
import           Foreign.C
import           Foreign.Ptr

foreign import ccall "webview_create" c_webview_create :: CInt -> Ptr () -> IO (Ptr ())
foreign import ccall "webview_destroy" c_webview_destroy :: Ptr () -> IO ()
foreign import ccall "webview_run" c_webview_run :: Ptr () -> IO ()
foreign import ccall "webview_set_title" c_webview_set_title :: Ptr () -> CString -> IO ()
foreign import ccall "webview_set_size" c_webview_set_size :: Ptr () -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "webview_navigate" c_webview_navigate :: Ptr () -> CString -> IO ()


newtype WebView = WebView (Ptr ())

type WebViewM = ReaderT WebView IO

runWebView :: Bool -> WebViewM () -> IO ()
runWebView isDebug m = do
  w <- c_webview_create (fromIntegral . fromEnum $ isDebug) nullPtr
  runReaderT m (WebView w)
  c_webview_run w
  c_webview_destroy w

title :: String -> WebViewM ()
title title = do
  (WebView w) <- ask
  liftIO . withCString title . c_webview_set_title $ w

data ResizeHint = ResizeNone | ResizeMin | ResizeMax | ResizeFix deriving (Enum)

size :: Int -> Int -> ResizeHint -> WebViewM ()
size width height hint = do
  (WebView w) <- ask
  liftIO $ c_webview_set_size w
                              (fromIntegral width)
                              (fromIntegral height)
                              (fromIntegral . fromEnum $ hint)

navigate :: String -> WebViewM ()
navigate url = do
  (WebView w) <- ask
  liftIO . withCString url . c_webview_navigate $ w
