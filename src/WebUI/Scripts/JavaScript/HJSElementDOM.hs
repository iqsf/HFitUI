----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Переменные для Языка JavaScript 
----------------------------------------------------------------

module WebUI.Scripts.JavaScript.HJSElementDOM
    ( getElementById 
    , addEventListener
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (strip)

import           WebUI.Scripts.HScript
import           WebUI.Scripts.JavaScript.HJSTypes



-- | Виджет скрипта JS
getElementById :: String 
               -> HLangJS 
getElementById eid =  
    HL $ "document.getElementById(\"" ++ eid ++ "\")"



-- Добавляем слушателя
addEventListener :: (BuilderHSL l) 
                 => String
                 -> l
                 -> Bool 
                 -> HSL HLangJS HLangJS 
addEventListener tp listener flag = do
    c <- ask
    hl <- return $ HL $ "window.addEventListener( \'" ++ tp ++ "\'," ++ (buildHSL_L c listener) ++ ","  ++ (buildHSL c flag) ++ " );" ++ (hbc_entryLine c)
    modify (:> hl)
    return hl

