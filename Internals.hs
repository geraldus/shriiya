module Internals
( ShriiyaHeritage(..)
, ShriiyaState
, tryAnimateShriiya
) where

import Control.Applicative( pure )
import Control.Applicative( (<$>), (<*>) )
import Control.Concurrent( forkIO )
import Control.Monad( liftM )
import FRP.Sodium
import GHCJS.Types
import GHCJS.Foreign( fromJSString )

import GDom.CommonDom

foldlMerge :: [Event a] -> Event a
foldlMerge (e:es) = foldl merge e es

class Host a where
    host :: a -> DocumentElement

-- | Animation - Текущее действие, которое производит сущность
data Animation = Animation
                     { startedAt :: Double -- ^ метка времени начала действия
                     , duration :: Double -- ^ длительность действия
                     -- , renderFn :: Double -> IO () -- ^ функция, которая меняет тело сущности
                     }

-- | ShriiyaHeritage - Наследие Шрийи
data ShriiyaHeritage = ShriiyaHeritage
                           { hostElement :: DocumentElement -- ^ тело - элемент документа
                           , defaultDelay :: Double -- ^ задержка для образов по умолчанию
                           , imageXPath :: String -- ^ х-путь для поиска образов
                           }
instance Host ShriiyaHeritage where
    host = hostElement

-- | ShriiyaImage - Образ Шрийи - логически связанные элемент-образ, его номер и его задержка
data ShriiyaImage = ShriiyaImage DocumentElement Int Double

-- | ShriiyaKey - это ключ, который логически связывает сам образ и его переключатель.
data ShriiyaKey = ShriiyaKey ShriiyaImage DocumentElement

-- | ShriiyaBody - Тело Шрийи
data ShriiyaBody = ShriiyaBody
                       { body :: DocumentElement -- ^ тело сущности, в котором она проявляет себя
                       , keys :: [ShriiyaKey]
                       , screen :: DocumentElement -- ^ экран
                       , switchesHost :: DocumentElement -- ^ держатель переключателей
                       , timerHost :: DocumentElement -- ^ держатель полосы времени
                       , timerBar :: DocumentElement -- ^ полоса времени
                       }
instance Host ShriiyaBody where
    host = body

-- | ShriiyaState – Состояние сущности Шрийи
data ShriiyaState = ShriiyaState
         { activeKey    :: Behaviour ShriiyaKey -- ^ текущее изображение
         , nextUpdateTS :: Behaviour Double -- ^ метка времени следуюего обновления, мсек
         , nextUpdateIn :: Behaviour Double -- ^ кол-во времени до следующего обновления, мсек
         , currentAnimation :: Behaviour (Maybe Animation)
         }

-- | ШРÏѦ - Шриия
-- Шриия - это реактивная сущность, для того, чтобы проявить себя ей нужно следующее:
-- + объект документа в котором проявляться
-- + входящие потоки событий переключателей изображений в паре с объектом переключателем
-- Что на выходе? Поведение, содержащее текущее состояние сущности
-- В потоке ввода-вывода организовывается прослушивание состояния сущности и осуществляется
-- обновление объектов документа
type Shriiya = Behaviour ShriiyaState

buildShriiyaBody :: ShriiyaHeritage -- наследие Шрийи
                 -> IO ShriiyaBody -- тело Шрийи
buildShriiyaBody h = do
    consgrp "Построение тела Шрийи"
    let hld = host h
    consobj hld

    qimgs <- hld `qsela` (imageXPath h)
    consgrp $ "Найдено образовпо х-пути `" ++ (imageXPath h) ++ "`: " ++ (show . length) qimgs ++ "."
    imgs <- mapM (\(e,n) -> consobj e >> makeShriiyaImage e n (defaultDelay h)) (zip qimgs [0,1..])
    consgre

    scr <- dcrel "div" >>= flip addcln "image-screen" >>= appcld hld
    mapM_ (\ (ShriiyaImage e n d) -> appcld scr e) imgs
    consgrp "Образы перенесены в специальное место"
    consobj scr >> consgre

    (sh, keys) <- buildShriiyaSwitches hld imgs

    th <- dcrel "div" >>= flip addcln "timer-holder" >>= appcld hld
    tb <- dcrel "div" >>= flip addcln "timer-bar" >>= appcld th
    consgrp "Создан блок для отображения таймера" >> consobj th >> consgre

    consgre
    return $ ShriiyaBody hld keys scr sh th tb

makeShriiyaImage :: DocumentElement -> Int -> Double -> IO ShriiyaImage
makeShriiyaImage e n def = do
    mdl <- e `getDataset` "delay"
    case mdl of
        Nothing -> setDataset e "delay" (show def) >> return (ShriiyaImage e n def)
        Just strd -> return (ShriiyaImage e n (read strd))

buildShriiyaSwitches :: DocumentElement -- основной объект (тело)
                     -> [ShriiyaImage] -- изображения
                     -> IO (DocumentElement, [ShriiyaKey]) -- пара держатель переключателей и ключи Шрийи
buildShriiyaSwitches h [] = conswarn "Нет образов!" >> return (h, [])
buildShriiyaSwitches h is = do
    sh <- dcrel "div" >>= flip addcln "switches"
    -- FIXME Ввести возможность добавления переключателей вперёд-назад
    --       А также возможность не оборажать цифровые
    --       Таким образом три варианта = Только первые, только вторые, все вместе
    ks <- mapM (\i -> do
                   s <- dcrel "div" >>= flip addcln "switch" >>= appcld sh
                   return $ ShriiyaKey i s
                   ) is
    h `appcld` sh

    consgrp "Созданы переключатели" >> consobj sh >> consgre

    return (sh, ks)

bindEvents :: Behaviour [(ShriiyaKey, Double)] -- состояние, в которое записываются события от мышки
           -> ([(ShriiyaKey, Double)] -> Reactive ()) -- функция управления состоянием
           -> [ShriiyaKey] -- ключи номер-образ-переключатель
           -> IO ()
bindEvents b f ks = do
    let mkClickH :: ShriiyaKey -> IO ()
        mkClickH k@(ShriiyaKey _ s) = do

            let clback :: DocumentEvent -> IO ()
                clback _ = do
                    -- FIXME Позже параметр можно использовать для опрделения типа события (левая-правая кнопка и т.д.)
                    pres <- sync $ sample b
                    ct <- currts
                    sync $ f ((k, ct):pres)

            addevlis s "click" clback >> return ()

    mapM_ mkClickH ks
    conslog "Нажатия на переключатели отслеживаются."

originateShriiya :: Event (ShriiyaKey, Double) -- поток событий, который сообщает необходимость сменить образ
               -> Event () -- поток событий, который сообщает необходимость сбросить анимацию
               -> ShriiyaKey -- первый образ
               -> Behaviour Double -- поведение, содержащее значение текущей метки времени, миллисекунды
               -> Reactive ShriiyaState
originateShriiya e_kalrm e_aalrm k1 b_curT = do
    -- Создаём саму сущность
    ct <- sample b_curT
    let ShriiyaKey (ShriiyaImage _ _ delay) _ = k1

    let e_curK  = fst <$> e_kalrm
        e_nxtUpdT = (\((ShriiyaKey (ShriiyaImage _ _ d) _),t) -> t + d) <$> e_kalrm

    rb_curK <- hold k1 e_curK
    rb_nxtUpdT <- hold (ct + delay) e_nxtUpdT
    let rb_nxtUpd = subtract <$> b_curT <*> rb_nxtUpdT

    let animDur = 300
        e_anim = (\(_, t) -> Just (Animation t animDur)) <$> e_kalrm
        e_animrst = (\_ -> Nothing) <$> e_aalrm
    rb_anim <- hold Nothing (merge e_anim e_animrst)

    return $ ShriiyaState rb_curK rb_nxtUpdT rb_nxtUpd rb_anim

tryAnimateShriiya :: ShriiyaHeritage -> IO ()
tryAnimateShriiya h = do
    -- Сначала мы строим тело для будущей сущности и создаём всё необходимое для её жизни
    -- В это число в первую очередь входит поток событий, который будет сообщать сущности
    -- какой образ выбрать текущим и когда кончится его время (метка времени следующего
    -- образа).
    -- Именно здесь будет создана нить, которая будет снабжать поток событий метками
    -- времени и проверять необходимость переключения образа.
    -- Именно здесь будут созданы переключатели, которые будут реагировать на щелчок мыши
    -- и отправлять сущности сообщение о необходимости смены образа.
    consgrp "Рождение Шрийи"

    body <- buildShriiyaBody h

    let ks = keys body
        kLen = length ks

    if kLen == 0
        then conserr $ "Образы по х-пути `" ++ (imageXPath h) ++ "` не найдены!"
        else do
            let k1 = head ks
                (ShriiyaKey (ShriiyaImage k1e _ _) _) = k1

            (e_alrm, fn_alrm)   <- sync newEvent -- поток сигналов для смены образа
            (b_curT, fn_curT) <- sync . newBehaviour =<< currts -- состояние текущего времени
            (e_animrst, fn_animrst) <- sync newEvent

            shriiya <- sync $ originateShriiya e_alrm e_animrst k1 b_curT

            (b_usrK, fn_usrK) <- sync $ newBehaviour [] -- Behaviour (ShriiyaKey, Double) | ключ - время
            bindEvents b_usrK fn_usrK ks

            -- FIXME Проверочная строчка, убрать
            sync $ fn_alrm (k1,0)

            let tmbr = timerBar body

            let next :: ShriiyaKey -> ShriiyaKey
                next (ShriiyaKey (ShriiyaImage _ n _) _) =
                    let nn = if n + 1 >= kLen
                        then 0
                        else n + 1
                    in ks !! nn

            let renderAnimation :: Animation -> IO ()
                renderAnimation a = do
                    pT <- sync $ sample b_curT
                    cT <- currts
                    cK <- sync $ sample (activeKey shriiya)
                    let fT = (startedAt a) + (duration a)
                        (ShriiyaKey (ShriiyaImage _ n _) _) = cK
                        fX = (negate . fromIntegral) n * 100

                    conslog $ "Время завершения анимации" ++ show fT

                    if fT <= cT
                        then k1e `styleMLeft` ((show fX) ++ "%")
                        else do
                            cX <- getCurMrg
                            let rT = fT - cT -- времени осталось до конца анимации
                                tT = fT - pT -- времени с прошлого раза до конца анимации
                                dT = tT - rT -- времени прошло с последнего обновления
                                v = (fX - cX) / tT -- скорость движения образа
                                nX = fX - v*dT
                            k1e `styleMLeft` ((show nX) ++ "%")
                    return ()
                        where
                            getCurMrg :: IO Double
                            getCurMrg = do
                                mrgstr <- getStyleMLeft k1e
                                return $ if mrgstr == [] || (last mrgstr) /= '%'
                                    then 0
                                    else read (init mrgstr)

            let loop = do
                conslog "Петля"

                cts <- currts
                conslog $ "Новая метка времени: " ++ (show cts) ++ "."

                nts <- sync $ sample (nextUpdateTS shriiya)
                -- let nts = 0
                conslog $ "Следующее обновление в " ++ (show nts) ++ "."

                ck  <- sync $ sample (activeKey shriiya)

                conslog "Проверяю время переключения образа"
                if cts >= nts
                    then sync $ fn_alrm (next ck, cts)
                    else return ()

                conslog "Проверяю события мыши"
                uk <- sync $ sample b_usrK
                if length uk /= 0
                    then do
                        let (nk, nt) = head uk
                        sync $ fn_alrm (nk, nt) >> fn_usrK []
                    else return ()

                conslog "Передаю новую метку времени"
                sync $ fn_curT cts

                ma <- sync $ sample (currentAnimation shriiya)
                case ma of
                    Nothing -> return ()
                    Just a -> do
                        renderAnimation a
                        if cts >= (startedAt a) + (duration a)
                            then sync $ fn_animrst ()
                            else return ()

                reqaf loop


            let renderTimer :: Double -> IO ()
                renderTimer t = do
                    (ShriiyaKey (ShriiyaImage _ _ delay) _) <- sync $ sample (activeKey shriiya)
                    let v = (100 * t / delay)
                        vs = show v ++ "%"
                    tmbr `styleWidth` vs
                    return ()

            let renderChange :: ShriiyaKey -> IO ()
                renderChange (ShriiyaKey (ShriiyaImage e n d) s) = do
                    let shost = (switchesHost body)
                    shost `nqsel` ".current" >>= (\p -> if isNull p then return () else p `remcln` "current" >> return ())
                    s `addcln` "current"
                    return ()

            conslog "Запускаю петляющую функцию в отдельной ните..."

            forkIO (reqaf loop)
            x <- sync $ listen ((value . activeKey) shriiya) renderChange
            y <- sync $ listen ((value . nextUpdateIn) shriiya) renderTimer
            conslog "Оживление завершено"
    consgre
