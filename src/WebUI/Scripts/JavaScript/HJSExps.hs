{-# LANGUAGE ScopedTypeVariables #-}
----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Выражения для языка JavaScript 
----------------------------------------------------------------

module WebUI.Scripts.JavaScript.HJSExps
    (-- HJsExp (..)

    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (strip)

import           WebUI.Scripts.HScript
import           WebUI.Scripts.JavaScript.HJSBuilder
import           WebUI.Scripts.JavaScript.HJSTypes
import           WebUI.Scripts.JavaScript.HJSVars



hbConf = defaultHBConfig



instance PRL.Num HLangJS where
    (HL x) + (HL y) = HL $ "(" ++ (buildHSL_L hbConf x) ++ " + " ++ (buildHSL_L hbConf y) ++ ")"
    _ + _ = error "HScript Error! Could not math operation \'+\' from class Num for HLangJS."

    (HL x) - (HL y) = HL $ "(" ++ (buildHSL_L hbConf x) ++ " - " ++ (buildHSL_L hbConf y) ++ ")"
    _ - _ = error "HScript Error! Could not math operation \'-\' from class Num for HLangJS."

    (HL x) * (HL y) = HL $ "(" ++ (buildHSL_L hbConf x) ++ " * " ++ (buildHSL_L hbConf y) ++ ")"
    _ * _ = error "HScript Error! Could not math operation \'*\' from class Num for HLangJS."

    negate      x = HL $ "((-1)*"     ++ (buildHSL_L hbConf x) ++ ")" 
    abs         x = HL $ "Math.abs("  ++ (buildHSL_L hbConf x) ++ ")"
    signum      x = HL $ "Math.sign(" ++ (buildHSL_L hbConf x) ++ ")"
    fromInteger x = HL $ x 



instance PRL.Fractional HLangJS where
    (HL x) / (HL y) = HL $ "(" ++ (buildHSL_L hbConf x) ++ " / " ++ (buildHSL_L hbConf y) ++ ")"
    _ / _ = error "HScript Error! Could not math operation \'/\' from class Num for HLangJS."
     
    fromRational x = HL $ buildHSL_L hbConf x



