module GDom.CommonDom where

import GHCJS.Foreign -- ( ToJSSting (..) )
import GHCJS.Types -- ( JSArray, JSRef, JSString,  isNull, isUndefined )

import Control.Concurrent( forkIO )
import Control.Concurrent.MVar( MVar, newEmptyMVar, takeMVar )


data DocumentElement_ = DocumentElement_
type DocumentElement = JSRef DocumentElement_


foreign import javascript safe "console.error($1)"
    js_conserr :: JSString -> IO ()
conserr :: String -> IO ()
conserr = js_conserr . toJSString

foreign import javascript safe "console.log($1)"
    js_conslog :: JSString -> IO ()
conslog :: String -> IO ()
conslog = js_conslog . toJSString

foreign import javascript safe "console.warn($1)"
    js_conswarn :: JSString -> IO ()
conswarn :: String -> IO ()
conswarn = js_conswarn . toJSString

foreign import javascript safe "console.log('%o', $1)"
    js_consobj :: JSRef a -> IO ()
consobj :: JSRef a -> IO ()
consobj = js_consobj

foreign import javascript safe "console.groupCollapsed($1);"
    js_consgrp :: JSString -> IO ()
consgrp :: String -> IO ()
consgrp = js_consgrp . toJSString

foreign import javascript safe "console.groupEnd();"
    js_consgre :: IO ()
consgre :: IO ()
consgre = js_consgre


foreign import javascript unsafe "$r = document.getElementById($1);"
    js_gebi :: JSString -> IO DocumentElement
ngebi :: String -> IO DocumentElement
ngebi = js_gebi . toJSString

foreign import javascript unsafe "$r = $1.querySelector($2);"
    js_qsel :: DocumentElement -> JSString -> IO DocumentElement
nqsel :: DocumentElement -> String -> IO DocumentElement
nqsel h = js_qsel h . toJSString

foreign import javascript unsafe "$r = $1.querySelectorAll($2);"
    js_qsela :: DocumentElement -> JSString -> IO (JSArray DocumentElement_)
qsela :: DocumentElement -> String -> IO [DocumentElement]
qsela h xp = js_qsela h (toJSString xp) >>= fromArray


foreign import javascript safe "$r = document.createElement($1)"
    js_dcrel :: JSString -> IO DocumentElement
dcrel :: String -> IO DocumentElement
dcrel = js_dcrel . toJSString


foreign import javascript safe "$1.classList.add($2);"
    js_addcln :: DocumentElement -> JSString -> IO ()
addcln :: DocumentElement -> String -> IO DocumentElement
addcln e cs = do
    let cl = map toJSString (words cs)
    mapM_ (js_addcln e) cl
    return e

foreign import javascript safe "$1.classList.remove($2);"
    js_remcln :: DocumentElement -> JSString -> IO ()
remcln :: DocumentElement -> String -> IO DocumentElement
remcln e cs = do
    let cl = map toJSString (words cs)
    mapM_ (js_remcln e) cl
    return e

foreign import javascript unsafe "$1.style.width = $2;"
    js_stylewidth :: DocumentElement -> JSString -> IO ()
-- FIXME Использовать вместо строки типизированные данные (CSSUnit)
styleWidth :: DocumentElement -> String -> IO DocumentElement
styleWidth e v = js_stylewidth e (toJSString v) >> return e

foreign import javascript safe "$1.style.marginLeft = $2;"
    js_styleMLeft :: DocumentElement -> JSString -> IO ()
-- FIXME Перейти от строк к специальному типу с параметрами
styleMLeft :: DocumentElement -> String -> IO DocumentElement
styleMLeft e v = js_styleMLeft e (toJSString v) >> return e

foreign import javascript unsafe "$r = $1.style[$2];"
    js_getStyleAttr :: DocumentElement -> JSString -> IO JSString
-- FIXME Очень небезопасная функция! Нужна проверка undefined для результата и null для объекта!
getStyleMLeft :: DocumentElement -> IO String
getStyleMLeft e = js_getStyleAttr e (toJSString "marginLeft") >>= return . fromJSString


foreign import javascript safe "$1.innerText = $2"
    js_setintext :: DocumentElement -> JSString -> IO ()
setintext :: DocumentElement -> String -> IO DocumentElement
setintext o t = js_setintext o (toJSString t) >> return o

-- FIXME Продумать логику обращения с набором данных элемента
foreign import javascript safe "$1.dataset[$2] = $3"
    js_setDataset :: DocumentElement -> JSString -> JSString -> IO ()
setDataset :: DocumentElement -> String -> String -> IO DocumentElement
setDataset e k v = js_setDataset e (toJSString k) (toJSString v) >> return e

foreign import javascript unsafe "$1.dataset[$2]"
     js_getDataset :: DocumentElement -> JSString -> IO (JSRef a)
getDataset :: DocumentElement -> String -> IO (Maybe String)
getDataset e k = js_getDataset e (toJSString k) >>= (\jr -> return $ if isUndefined jr then Nothing else Just (fromJSString jr))


foreign import javascript safe "r = $1.appendChild($2);"
    js_appcld :: DocumentElement -> DocumentElement -> IO DocumentElement
appcld :: DocumentElement -> DocumentElement -> IO DocumentElement
appcld h e = js_appcld h e >> return e


foreign import javascript safe "$1.addEventListener($2, h$makeMVarListener($3,false,false,false));"
    js_addevlis :: DocumentElement -> JSString -> JSObject (MVar DocumentEvent) -> IO ()
-- foreign import javascript safe "$1.addEventListener($2, $3);"
--     js_addevlis' :: DocumentElement -> JSString -> (JSFun (DocumentEvent -> IO ())) -> IO ()
addevlis :: DocumentElement -> String -> (DocumentEvent -> IO ()) -> IO DocumentElement
addevlis o et f = do
    mv <- newEmptyMVar :: IO (MVar DocumentEvent)

    let loop = do
        takeMVar mv >>= f
        reqaf loop

    forkIO (reqaf loop)
    js_addevlis o (toJSString et) (mvarRef mv)
    return o
-- addevlis' :: (JSRef DocumentElement_) -> String -> (DocumentEvent -> IO ()) -> IO DocumentElement
-- addevlis' o et f = do
--     ch <- newChan :: IO (Chan DocumentEvent)
--     let loop = do
--         readChan ch >>= f
--         reqaf loop
--
--     forkIO (reqaf loop)
--     -- FIXME: Сообщить о том, что DomRetain ведёт себя некорректно
--     cb <- asyncCallback1 {-(DomRetain (castRef o))-} AlwaysRetain (\e -> writeChan ch e)
--     js_addevlis' o (toJSString et) cb
--     return o
-- addevlis'' :: (JSRef DocumentElement_) -> String -> (DocumentEvent -> IO ()) -> IO DocumentElement
-- addevlis'' o et f = do
--     ch <- newChan :: IO (Chan DocumentEvent)
--     forkIO (forever $ readChan ch >>= f)
--     -- FIXME: Сообщить о том, что DomRetain ведёт себя некорректно
--     cb <- asyncCallback1 {-(DomRetain (castRef o))-} AlwaysRetain (\e -> writeChan ch e)
--     js_addevlis' o (toJSString et) cb
--     return o


foreign import javascript safe "$r = new Date().getTime();"
    js_currts :: IO Double
currts :: IO Double
currts = js_currts

foreign import javascript safe "requestAnimationFrame($1);"
    js_reqaf :: JSFun (IO ()) -> IO ()
reqaf :: IO () -> IO ()
reqaf f = do
    -- FIXME Использовать другой способ удержания
    cb <- asyncCallback AlwaysRetain f
    js_reqaf cb


data DocumentEvent_ = AnyEvent
                    --  | MouseEvent { x :: Int, y :: Int }
type DocumentEvent = JSRef DocumentEvent_
