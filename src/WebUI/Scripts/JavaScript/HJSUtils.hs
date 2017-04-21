----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Утилиты генератора языка JavaScript 
----------------------------------------------------------------

module WebUI.Scripts.JavaScript.HJSUtils
    ( smartTrim, smartTrimStr
    , smartTab , smartTabStr
    , ujs
    , upjs
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           System.IO.Unsafe                               (unsafePerformIO)

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (strip)

import qualified Data.Text.Lazy                              as DTL
import           Data.Int

import           WebUI.Scripts.HScript
import           WebUI.Scripts.JavaScript.HJSTypes
import           WebUI.Scripts.JavaScript.HJSBuilder




-- | Умная обрезка текста кода после Julius (String)
smartTrimStr :: String 
             -> String
smartTrimStr txt = DTL.unpack $ smartTrim $ DTL.pack txt



-- | Умная обрезка текста кода после Julius
smartTrim :: DTL.Text 
          -> DTL.Text
smartTrim txt = 
    let linesTxt = DTL.lines txt in
    DTL.unlines $ dropWhole linesTxt $ findCount linesTxt maxCounter
    where 
        findCount :: [DTL.Text] -> Int64 -> Int64
        findCount (x:xs) counter = let len = lenSP x counter in 
                                   if len < counter 
                                   then findCount xs len 
                                   else findCount xs counter
        findCount []     counter = if counter == maxCounter 
                                   then 0 
                                   else counter

        maxCounter :: Int64
        maxCounter = 1000000  

        lenSP :: DTL.Text -> Int64 -> Int64
        lenSP txt counter = let res = lenSP_ txt counter in if res < 0
                                                            then 0
                                                            else res
 
        lenSP_ :: DTL.Text -> Int64 -> Int64
        lenSP_ txt counter = if (DTL.length txt) == 0 || (DTL.length txt) == (DTL.length $ DTL.takeWhile (==' ') txt)
                             then counter
                             else  DTL.length $ DTL.takeWhile (==' ') txt

        dropWhole :: [DTL.Text] -> Int64 -> [DTL.Text]
        dropWhole (x:xs) count = if (DTL.length x) > count 
                                 then (DTL.drop count x):(dropWhole xs count)
                                 else (dropWhole xs count)
        dropWhole []     _     = [] 





-- | Умная табуляция кода (String)
smartTabStr :: String
            -> String
            -> String
smartTabStr txt tab = DTL.unpack $ smartTab (DTL.pack txt) (DTL.pack tab)


 
-- | Умная табуляция кода
smartTab :: DTL.Text 
         -> DTL.Text
         -> DTL.Text
smartTab txt tab = 
    let linesTxt = DTL.lines txt in
    DTL.unlines $ tabWhole linesTxt $ tab
    where 
        tabWhole :: [DTL.Text] -> DTL.Text -> [DTL.Text]
        tabWhole (x:xs) tb = if (DTL.length x) > 0 
                             then (DTL.append tb x):(tabWhole xs tb)
                             else x:(tabWhole xs tb)
        tabWhole []     _  = [] 


-- | Распаковать значение из монадгного трансформера с конфигурацией 
-- по умолчанию и пустым исходным состоянияем
ujs :: HSL HLangJS HLangJS
    -> HLangJS
ujs srcHSL =  
    unsafePerformIO $ do
        (a, s, log) <- runRWST srcHSL defaultHBConfig HLangJS
        return a 



-- | Распаковать значение из монадгного трансформера с конфигурацией 
-- по умолчанию и пустым исходным состоянияем
upjs :: HSL HLangJS HLangJS
     -> HBConfig
     -> HLangJS
     -> HLangJS
upjs srcHSL hbConf st =  
    unsafePerformIO $ do
        (a, s, log) <- runRWST srcHSL hbConf st
        return a


 
