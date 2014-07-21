module Main where

import GHCJS.Types( isNull )

import GDom.CommonDom( conserr, ngebi )
import Internals( ShriiyaHeritage(..), tryAnimateShriiya )

-- Первое воплощение будет очень, очень простым
-- Далее следует придумать безопасный код, который проверяет
-- специальные значения null и undefined
-- Скорее всего нужно будет описать монаду

main :: IO ()
main = do
    let tid = "shriiya"
    ns <- ngebi tid
    case isNull ns of
        False -> tryAnimateShriiya $ ShriiyaHeritage ns 15000 ".image"
        True -> conserr $ "Не найден элемент `" ++ tid ++ "`."
