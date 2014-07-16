{-# LANGUAGE JavaScriptFFI #-}
module Main where

import GHCJS.Foreign
import GHCJS.Types

import Control.Applicative( pure )
import GHC.Conc( forkIO )
import Control.Applicative( (<$>), (<*>) )
import Control.Concurrent.Chan( Chan, newChan, readChan, writeChan )
import Control.Monad( forever, liftM )
import FRP.Sodium

-- Первое воплощение будет очень, очень простым
-- Далее следует придумать безопасный код, который проверяет
-- специальные значения null и undefined
-- Скорее всего нужно будет описать монаду


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

foreign import javascript safe "console.group($1);"
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
nqsel o = js_qsel o . toJSString

foreign import javascript unsafe "$r = $1.querySelectorAll($2);"
    js_qsela :: DocumentElement -> JSString -> IO (JSArray DocumentElement_)
qsela :: DocumentElement -> String -> IO [DocumentElement]
qsela o xp = js_qsela o (toJSString xp) >>= fromArray


foreign import javascript safe "$r = document.createElement($1)"
    js_dcrel :: JSString -> IO DocumentElement
dcrel = js_dcrel . toJSString


foreign import javascript safe "$1.classList.add($2);"
    js_addcln :: DocumentElement -> JSString -> IO ()
addcln :: DocumentElement -> String -> IO DocumentElement
addcln o cs = do
    let cl = map toJSString (words cs)
    mapM_ (js_addcln o) cl
    return o

foreign import javascript safe "$1.innerText = $2"
    js_setintext :: DocumentElement -> JSString -> IO ()
setintext :: DocumentElement -> String -> IO DocumentElement
setintext o t = js_setintext o (toJSString t) >> return o

foreign import javascript safe "r = $1.appendChild($2);"
    js_appcld :: DocumentElement -> DocumentElement -> IO DocumentElement
appcld :: DocumentElement -> DocumentElement -> IO DocumentElement
appcld = js_appcld


foreign import javascript safe "$1.addEventListener($2, $3);"
    js_addevlis :: DocumentElement -> JSString -> (JSFun (DocumentEvent -> IO ())) -> IO ()
addevlis :: (JSRef DocumentElement_) -> String -> (DocumentEvent -> IO ()) -> IO DocumentElement
addevlis o et f = do
    ch <- newChan :: IO (Chan DocumentEvent)
    forkIO (forever $ readChan ch >>= f)
    -- FIXME: Сообщить о том, что DomRetain ведёт себя некорректно
    cb <- asyncCallback1 {-(DomRetain (castRef o))-} AlwaysRetain (\e -> writeChan ch e)
    js_addevlis o (toJSString et) cb
    return o


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

foldlMerge :: [Event a] -> Event a
foldlMerge (e:es) = foldl merge e es


data DocumentEvent_ = AnyEvent
                    --  | MouseEvent { x :: Int, y :: Int }
type DocumentEvent = JSRef DocumentEvent_

type EventSource = ( [Event DocumentEvent] , DocumentElement )

-- | SHRIIYAState – Состояние сущности ШГДЯ
data SHRIIYAState = SHRIIYAState
         { activeImage :: Behaviour DocumentElement -- ^ текущее изображение
         , nextUpdateTS :: Behaviour Double -- ^ метка времени следуюего обновления, мсек
         , nextUpdateIn :: Behaviour Double -- ^ кол-во времени до следующего обновления, мсек
         }

-- | ШГДЯ
-- ШГДЯ - это реактивная сущность, для того, чтобы проявить себя ей нужно следующее:
-- + объект документа в котором проявляться
-- + входящие потоки событий переключателей изображений в паре с объектом переключателем
-- Что на выходе? Поведение, содержащее текущее состояние сущности
-- В потоке ввода-вывода организовывается прослушивание состояния сущности и осуществляется
-- обновление объектов документа
type SHRIIYA = Behaviour SHRIIYAState

buildSHRIIYABody :: DocumentElement -- объект, содержащий образы
                -> String -- х-путь по которому искать изображения
                -> ((DocumentElement, Double) -> Reactive ()) -- функция для оповещения сущности
                -> Double -- задержка в миллисекундах
                -> IO DocumentElement
buildSHRIIYABody h xp f d = do
    consgrp "Построение тела ШГДЯ"
    consobj h

    imgs <- h `qsela` xp
    conslog $ "Поиск образов по х-пути: " ++ xp
    conslog $ "Найдено образов:" ++ (show . length) imgs
    mapM_ consobj imgs

    buildSwitches h imgs >>= bindEvents f d

    consgre
    -- FIXME Небезопасное получение первого элемента
    return $ head imgs
  where
    buildSwitches :: DocumentElement -- основной объект (тело)
                  -> [DocumentElement] -- изображения
                  -> IO [(DocumentElement, DocumentElement)] -- переключатели
    buildSwitches _ [] = conslog "Нет образов!" >> return []
    buildSwitches h is = do
        sh <- dcrel "div" >>= flip addcln "switches"
        -- FIXME Ввести возможность добавления переключателей вперёд-назад
        --       А также возможность не оборажать цифровые
        --       Таким образом три варианта = Только первые, только вторые, все вместе
        sws <- mapM (\(_, n) -> do
                        so <- dcrel "div"
                        so `addcln` "switch" >>= appcld sh
                        so `setintext` (show n)) (zip is [1..])
        h `appcld` sh
        return (zip is sws)
    bindEvents :: ((DocumentElement, Double) -> Reactive ()) -- функция для оповещения сущности
               -> Double -- задержка в миллисекундах
               -> [(DocumentElement, DocumentElement)] -- пары образ-переключатель
               -> IO ()
    bindEvents f d isps = do
        mapM_ (\(i,s) -> addevlis s "click" (\_ -> findNextNNotify i f d)) isps
        where findNextNNotify :: DocumentElement -- выбранный образ
                              -> ((DocumentElement, Double) -> Reactive ()) -- функция для оповещения
                              -> Double -- задержка в миллисекундах
                              -> IO ()
              findNextNNotify i f d = sync . f . (,) i . (+) d =<< currts

animateSHRIIYA :: Event (DocumentElement, Double) -- поток событий, который сообщает необходимость сменить образ
              -> DocumentElement -- первый образ
              -> Double -- задержка для отображения каждого изображения, миллисекунды
              -> Behaviour Double -- поведение, содержащее значение текущей метки времени, миллисекунды
              -> Int -- кол-во повторений всех изображений (0 для бесконечного повторения), временно не используется.
              -> Reactive SHRIIYAState
animateSHRIIYA ae ii d bct lc = do
    -- Создаём саму сущность
    ct <- sample bct
    let e_ct = value bct
    (b_nut, fn_nut) <- newBehaviour (ct + d)
    (b_ai, _) <- newBehaviour ii
    let e_ai  = snapshot (\(ni, _) _ -> ni) ae b_ai
        e_nut = snapshot (\(_, nt) _ -> nt) ae b_nut
    rb_ai <- hold ii e_ai
    rb_nut <- hold (ct + d) e_nut
    let rb_nui = subtract <$> bct <*> rb_nut
    return $ SHRIIYAState rb_ai rb_nut rb_nui

tryAnimateSHRIIYA :: String -> IO ()
tryAnimateSHRIIYA str = do
    -- Сначала мы строим тело для будущей сущности и создаём всё необходимое для её жизни
    -- В это число в первую очередь входит поток событий, который будет сообщать сущности
    -- какой образ выбрать текущим и когда кончится его время (метка времени следующего
    -- образа).
    -- Именно здесь будет создана нить, которая будет снабжать поток событий метками
    -- времени и проверять необходимость переключения образа.
    -- Именно здесь будут созданы переключатели, которые будут реагировать на щелчок мыши
    -- и отправлять сущности сообщение о необходимости смены образа.
    consgrp "Рождение ШГДЯ"
    ne <- ngebi str
    case (isNull ne) of
        True -> conserr $ "Не найден элемент '" ++ str ++ "'"
        False -> do
            (e_a, fn_a)   <- sync newEvent -- поток сигналов для смены образа
            (b_ct, fn_ct) <- sync . newBehaviour =<< currts -- состояние текущего времени
            let delay = 3 * 1000
            ii <- buildSHRIIYABody ne ".image" fn_a delay
            ii `addcln` "current"
            shriiya <- sync $ animateSHRIIYA e_a ii delay b_ct 0

            let findnext :: IO (DocumentElement, Double)
                findnext = do
                    nt <- liftM ((+) delay) currts
                    nn <- ne `nqsel` (".current + " ++ ".image")
                    if isNull nn
                        then ne `nqsel` ".image" >>= return . flip (,) nt
                        else return (nn, nt)

            let loop = do
                cts <- currts
                nts <- sync $ sample (nextUpdateTS shriiya)
                if cts >= nts
                    then do
                        next <- findnext
                        sync $ fn_a next >> fn_ct cts
                    else sync $ fn_ct cts
                reqaf loop

            forkIO (reqaf loop)
            x <- sync $ listen ((value . activeImage) shriiya) (\o -> consobj o)
            y <- sync $ listen ((value . nextUpdateIn) shriiya) (\t -> ngebi "trace" >>= flip setintext (show t) >> return ())
            conslog "Оживление завершено"
            consobj ne
    consgre


targetId :: String
targetId = "shriiya"


main = do
    let tid = targetId
    tryAnimateSHRIIYA tid
