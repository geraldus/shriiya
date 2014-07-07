{-# LANGUAGE JavaScriptFFI #-}
{- # LANGUAGE JavaScriptFFI #-}
module Main where


-- import Data.String( IsString )

import GHCJS.Foreign
import GHCJS.Types

import Control.Applicative( pure )
import FRP.Sodium

-- Первое воплощение будет очень, очень простым
-- Далее следует придумать безопасный код, который проверяет специальные значения null и undefined
-- Скорее всего нужно будет описать монаду


data DocumentObject_ = DocumentObject_
type DocumentObject = JSRef DocumentObject_


foreign import javascript safe "console.error($1)"
    js_conserr :: JSString -> IO ()
conserr :: String -> IO ()
conserr = js_conserr . toJSString

foreign import javascript safe "console.log($1)"
    js_conslog :: JSString -> IO ()
conslog :: String -> IO ()
conslog = js_conslog . toJSString

foreign import javascript safe "console.log('%o', $1)"
    js_consobj :: JSRef a -> IO ()
consobj :: JSRef a -> IO ()
consobj = js_consobj


foreign import javascript unsafe "$r = document.getElementById($1);"
    js_gebi :: JSString -> IO DocumentObject
ngebi :: String -> IO DocumentObject
ngebi = js_gebi . toJSString

data DocumentEvent_ = AnyEvent
                    | MouseEvent { x :: Int, y :: Int }
type DocumentEvent = JSRef DocumentEvent_

type EventSource = ( [DocumentEvent] , DocumentObject )

-- | ShGDYaState – Состояние сущности ШГДЯ
data ShGDYaState = ShGDYaState
        { -- host :: DocumentObject -- ^ Объект документа в котором живет сущность
        -- , 
        switches :: Event [EventSource] -- ^ переключатели изображений с ихнеми потоками событий
        , activeImage :: Event DocumentObject -- ^ текущее изображение
        , nextUpdateTS :: Event Int -- ^ метка времени следуюего обновления, мсек
        , nextUpdateIn :: Event Int -- ^ кол-во времени до следующего обновления, мсек
        }

-- | ШГДЯ
-- ШГДЯ - это реактивная сущность, для того, чтобы проявить себя ей нужно следующее:
-- + объект документа в котором проявляться
-- + входящие потоки событий переключателей изображений в паре с объектом переключателем
-- Что на выходе? Поведение, содержащее текущее состояние сущности
-- В потоке ввода-вывода организовывается прослушивание состояния сущности и осуществляется
-- обновление объектов документа
type ShGDYa = Behaviour ShGDYaState

animateShGDYa :: DocumentObject -- объект документа, в котором расположиться сущность
              -> [EventSource] -- упрорядоченный список переключателей - источников событий
              -> Int -- задержка для отображения каждого изображения, миллисекунды
              -> Behaviour Int -- поведение, содержащее значение текущей метки времени, миллисекунды
              -> Int -- кол-во повторений всех изображений (0 для бесконечного повторения), временно не используется.
              -> Reactive ShGDYa
animateShGDYa h [] _ _ _ = do
    nbf <- newBehaviour $ ShGDYaState never never never never
    (return . fst) nbf
animateShGDYa h sl d bct lc = do
    -- Создаём саму сущность
    ct <- sample bct
    (b_sl, fn_sl) <- newBehaviour sl
    (b_ai, fn_ai) <- newBehaviour ((snd . head) sl)
    (b_nut, fn_nut) <- newBehaviour (ct + d)
    (b_nur, fn_nur) <- newBehaviour d
    (b_ess, fn_ess) <- newBehaviour $ ShGDYaState (value b_sl) (value b_ai) (value b_nut) (value b_nur) 
    -- Теперь, зададим реакции на потоки событий
    return b_ess

targetId :: String
targetId = "shgdya"

tryAnimate :: String -> IO ()
tryAnimate str = do
    ne <- ngebi targetId
    case (isNull ne) of
        True -> conserr $ "Не найден элемент " ++ str
        False -> conslog "wip" 


main = do
    let tid = targetId
    tryAnimate tid
-- Поиск элемента, элементов
-- Настройка стилей
-- Создание реактивных событий
-- Оживление ШГДЯ
-- Связка действительных событий с исполнителями/управленцами
