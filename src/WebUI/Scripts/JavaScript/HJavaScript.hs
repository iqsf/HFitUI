----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Язык JavaScript 
----------------------------------------------------------------

module WebUI.Scripts.JavaScript.HJavaScript
    ( module HJavaScriptBuilder
    , module HJavaScriptTypes
    , module HJavaScriptVars
    , module HJavaScriptExps
    , module HJavaScriptMath
    , module HJavaScriptFunction
    , module HJavaScriptElementDOM
    , hjs
    , ujs
    , upjs
    , hjsBR
    , jsFinish
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (strip)

import qualified Data.Text.Lazy                              as DTL
import           Data.Int

import           Control.Monad.RWS                           as ConMonRWS

import           System.IO.Unsafe                               (unsafePerformIO)

import           Text.Blaze.Html5                

import           Text.Hamlet
import           Text.Lucius
import           Text.Cassius
import           Text.Julius

import           WebUI.Scripts.HScript
import           WebUI.Scripts.JavaScript.HJSUtils              (smartTrim, ujs, upjs)
import           WebUI.Scripts.JavaScript.HJSBuilder         as HJavaScriptBuilder
import           WebUI.Scripts.JavaScript.HJSTypes           as HJavaScriptTypes
import           WebUI.Scripts.JavaScript.HJSVars            as HJavaScriptVars
import           WebUI.Scripts.JavaScript.HJSExps            as HJavaScriptExps
import           WebUI.Scripts.JavaScript.HJSMath            as HJavaScriptMath
import           WebUI.Scripts.JavaScript.HJSFunction        as HJavaScriptFunction
import           WebUI.Scripts.JavaScript.HJSElementDOM      as HJavaScriptElementDOM



-- | Подготовка JavaScript
prepareJS b = renderJavascriptUrl undefined b



-- | Виджет скрипта JS
hjs :: JavascriptUrl b 
    -> HSL HLangJS HLangJS 
hjs js = do 
    hl <- return $ HL $ smartTrim $ do prepareJS js
    modify (:> hl)
    return hl



-- | Детектор WebGL
hjsBR :: HSL HLangJS HLangJS 
hjsBR = do  
    c <- ask
    hl <- return $ HL $ " " ++ (hbc_entryLine c)
    modify (:> hl)
    return hl


jsFinish :: HSL HLangJS HLFinish
jsFinish = do
    return HLFinish



