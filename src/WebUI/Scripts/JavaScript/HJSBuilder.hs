----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Билдер для языка JavaScript 
----------------------------------------------------------------

module WebUI.Scripts.JavaScript.HJSBuilder
    () where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
--import           Data.String.Utils                              (strip)
import qualified Data.Text.Lazy                              as DTL

import           WebUI.Scripts.HScript



instance BuilderHSL Int where
    buildHSL hbConf v = show v 
    buildHSL_L = buildHSL
    buildHSL_R = buildHSL


instance BuilderHSL Integer where
    buildHSL hbConf v = show v 
    buildHSL_L = buildHSL
    buildHSL_R = buildHSL


instance BuilderHSL Rational where
    buildHSL hbConf v = show v 
    buildHSL_L = buildHSL
    buildHSL_R = buildHSL


instance BuilderHSL Double where
    buildHSL hbConf v = show v
    buildHSL_L = buildHSL
    buildHSL_R = buildHSL



instance BuilderHSL Bool where
    buildHSL hbConf v = strBool v
        where
            lwr       = map toLower
            strBool f = lwr $ show f
    buildHSL_L = buildHSL
    buildHSL_R = buildHSL



instance BuilderHSL String where
    buildHSL hbConf v = v 
    buildHSL_L = buildHSL
    buildHSL_R = buildHSL



instance BuilderHSL DTL.Text where
    buildHSL hbConf v = DTL.unpack v 
    buildHSL_L = buildHSL
    buildHSL_R = buildHSL



