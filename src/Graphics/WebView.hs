module Graphics.WebView
  ( WebView
  , WebViewM
  , runWebView
  , title
  , size
  , navigate
  , bind
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
foreign import ccall "webview_bind" c_webview_bind :: Ptr () -> CString -> FunPtr (CString -> CString -> Ptr () -> IO ()) -> Ptr () -> IO ()
foreign import ccall "webview_return" c_webview_return :: Ptr () -> CString -> CInt -> CString -> IO ()
foreign import ccall "wrapper" mkBindCallback :: (CString -> CString -> Ptr () -> IO ()) -> IO (FunPtr (CString -> CString -> Ptr () -> IO ()))


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

bind :: String -> (String -> IO (Either String String)) -> WebViewM ()
bind name callback = do
  (WebView w) <- ask
  callback'   <- liftIO . mkBindCallback $ \seq req _ -> do
    req' <- peekCString req
    res  <- callback req'
    case res of
      Left err -> do
        withCString err $ c_webview_return w seq 1
      Right res' -> do
        withCString res' $ c_webview_return w seq 0
  liftIO . withCString name $ \name' -> c_webview_bind w name' callback' nullPtr
