module Graphics.WebView
  ( WebView
  , WebViewM
  , runWebView
  , title
  , size
  , navigate
  , initJS
  , evalJS
  , bind
  , ResizeHint(..)
  ) where

import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , liftIO
                                                , runReaderT
                                                )
import qualified Data.Text                     as T
import           Foreign.C
import           Foreign.Ptr

foreign import ccall "webview_create" c_webview_create :: CInt -> Ptr () -> IO (Ptr ())
foreign import ccall "webview_destroy" c_webview_destroy :: Ptr () -> IO ()
foreign import ccall "webview_run" c_webview_run :: Ptr () -> IO ()
foreign import ccall "webview_set_title" c_webview_set_title :: Ptr () -> CString -> IO ()
foreign import ccall "webview_set_size" c_webview_set_size :: Ptr () -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "webview_navigate" c_webview_navigate :: Ptr () -> CString -> IO ()
foreign import ccall "webview_init" c_webview_init :: Ptr () -> CString -> IO ()
foreign import ccall "webview_eval" c_webview_eval :: Ptr () -> CString -> IO ()
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

title :: T.Text -> WebViewM ()
title title = do
  (WebView w) <- ask
  liftIO . withCString (T.unpack title) . c_webview_set_title $ w

data ResizeHint = ResizeNone | ResizeMin | ResizeMax | ResizeFix deriving (Enum)

size :: Int -> Int -> ResizeHint -> WebViewM ()
size width height hint = do
  (WebView w) <- ask
  liftIO $ c_webview_set_size w
                              (fromIntegral width)
                              (fromIntegral height)
                              (fromIntegral . fromEnum $ hint)

navigate :: T.Text -> WebViewM ()
navigate url = do
  (WebView w) <- ask
  liftIO . withCString (T.unpack url) $ c_webview_navigate w

initJS :: T.Text -> WebViewM ()
initJS code = do
  (WebView w) <- ask
  liftIO . withCString (T.unpack code) $ c_webview_init w

evalJS :: T.Text -> WebViewM ()
evalJS code = do
  (WebView w) <- ask
  liftIO . withCString (T.unpack code) $ c_webview_eval w

bind :: T.Text -> (T.Text -> IO (Either T.Text T.Text)) -> WebViewM ()
bind name callback = do
  (WebView w) <- ask
  callback'   <- liftIO . mkBindCallback $ \seq req _ -> do
    req' <- T.pack <$> peekCString req
    res  <- callback req'
    case res of
      Left err -> do
        withCString (T.unpack err) $ c_webview_return w seq 1
      Right res' -> do
        withCString (T.unpack res') $ c_webview_return w seq 0
  liftIO . withCString (T.unpack name) $ \name' ->
    c_webview_bind w name' callback' nullPtr
