{-# LANGUAGE JavaScriptFFI #-}
{- # LANGUAGE JavaScriptFFI #-}
module Main where


-- import Data.String( IsString )

import GHCJS.Foreign
import GHCJS.Types

-- Первое воплощение будет очень, очень простым
-- Далее следует придумать безопасный код, который проверяет специальные значения null и undefined
-- Скорее всего нужно будет описать монаду


data DocumentObject_ = DocumentObject_
type DocumentObject = JSRef DocumentObject_


foreign import javascript unsafe "console.log($1)"
    js_conslog :: JSString -> IO ()
conslog :: String -> IO ()
conslog = js_conslog . toJSString

foreign import javascript unsafe "console.log('%o', $1)"
    js_consobj :: JSRef a -> IO ()
consobj :: JSRef a -> IO ()
consobj = js_consobj


foreign import javascript unsafe "$r = document.getElementById($1);"
    js_gebi :: JSString -> IO DocumentObject
ugebi :: String -> IO DocumentObject
ugebi = js_gebi . toJSString


targetId :: String
targetId = "shgdya"

tryAnimate :: String -> IO ()
tryAnimate str = do
    me <- ugebi "error" -- targetId
    consobj me


main = do
    let tid = targetId
    tryAnimate tid
-- Поиск элемента, элементов
-- Настройка стилей
-- Создание реактивных событий
-- Оживление ШГДЯ
-- Связка действительных событий с исполнителями/управленцами
