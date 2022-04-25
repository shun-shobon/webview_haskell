module Graphics.WebView
  ( WebView,
    WebViewM,
    runWebView,
    title,
    Size (..),
    size,
    sizeMin,
    sizeMax,
    navigate,
    initJS,
    evalJS,
    bind,
  )
where

import Control.Concurrent (forkIO)
import Control.Monad.Reader
  ( ReaderT,
    asks,
    liftIO,
    runReaderT,
  )
import Data.IORef
  ( IORef,
    atomicModifyIORef,
    newIORef,
    readIORef,
  )
import qualified Data.Text as T
import Foreign.C
import Foreign.Ptr

foreign import ccall "webview_create" c_webview_create :: CInt -> Ptr () -> IO (Ptr ())

foreign import ccall "webview_destroy" c_webview_destroy :: Ptr () -> IO ()

foreign import ccall "webview_run" c_webview_run :: Ptr () -> IO ()

foreign import ccall "webview_set_title" c_webview_set_title :: Ptr () -> CString -> IO ()

foreign import ccall "webview_set_size" c_webview_set_size :: Ptr () -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "webview_navigate" c_webview_navigate :: Ptr () -> CString -> IO ()

foreign import ccall "webview_init" c_webview_init :: Ptr () -> CString -> IO ()

foreign import ccall "webview_eval" c_webview_eval :: Ptr () -> CString -> IO ()

type Callback = CString -> CString -> Ptr () -> IO ()

foreign import ccall "webview_bind" c_webview_bind :: Ptr () -> CString -> FunPtr Callback -> Ptr () -> IO ()

foreign import ccall "webview_return" c_webview_return :: Ptr () -> CString -> CInt -> CString -> IO ()

foreign import ccall "wrapper" mkBindCallback :: Callback -> IO (FunPtr Callback)

data WebView = WebView
  { webViewPtr :: Ptr (),
    webViewBindings :: IORef [FunPtr Callback]
  }

type WebViewM = ReaderT WebView IO

runWebView :: Bool -> WebViewM () -> IO ()
runWebView isDebug m = do
  ptr <- c_webview_create (fromIntegral . fromEnum $ isDebug) nullPtr
  bindings <- newIORef []
  runReaderT m (WebView ptr bindings)
  c_webview_run ptr
  c_webview_destroy ptr
  readIORef bindings >>= mapM_ freeHaskellFunPtr

title :: T.Text -> WebViewM ()
title title =
  asks webViewPtr
    >>= liftIO
      . withCString (T.unpack title)
      . c_webview_set_title

data Size = Size
  { sizeWidth :: Int,
    sizeHeight :: Int
  }

size :: Size -> Bool -> WebViewM ()
size (Size width height) resizable = do
  ptr <- asks webViewPtr
  let hint = if resizable then 0 else 3
  liftIO $
    c_webview_set_size ptr (fromIntegral width) (fromIntegral height) hint

sizeMin :: Size -> WebViewM ()
sizeMin (Size width height) = do
  ptr <- asks webViewPtr
  liftIO $ c_webview_set_size ptr (fromIntegral width) (fromIntegral height) 1

sizeMax :: Size -> WebViewM ()
sizeMax (Size width height) = do
  ptr <- asks webViewPtr
  liftIO $ c_webview_set_size ptr (fromIntegral width) (fromIntegral height) 2

navigate :: T.Text -> WebViewM ()
navigate url =
  asks webViewPtr >>= liftIO . withCString (T.unpack url) . c_webview_navigate

initJS :: T.Text -> WebViewM ()
initJS code =
  asks webViewPtr >>= liftIO . withCString (T.unpack code) . c_webview_init

evalJS :: T.Text -> WebViewM ()
evalJS code =
  asks webViewPtr >>= liftIO . withCString (T.unpack code) . c_webview_eval

bind :: T.Text -> (T.Text -> IO (Either T.Text T.Text)) -> WebViewM ()
bind name callback = do
  ptr <- asks webViewPtr
  bindings <- asks webViewBindings
  callback' <- liftIO . mkBindCallback $ \seq req _ -> do
    req' <- T.pack <$> peekCString req
    res <- callback req'
    case res of
      Left err -> do
        withCString (T.unpack err) $ c_webview_return ptr seq 1
      Right res' -> do
        withCString (T.unpack res') $ c_webview_return ptr seq 0
  liftIO . withCString (T.unpack name) $ \name' ->
    c_webview_bind ptr name' callback' nullPtr
  liftIO . atomicModifyIORef bindings $ \bindings' ->
    (callback' : bindings', ())
