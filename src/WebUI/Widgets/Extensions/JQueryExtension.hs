----------------------------------------------------------------
-- Модуль приложения
-- Расширение графического интерфейса (ExtensionUI)
-- Библиотека jQuery
----------------------------------------------------------------

module WebUI.Widgets.Extensions.JQueryExtension
    ( jQueryExtension
    ) where

-- Импорт модулей
import           Prelude                         as PRL

import qualified Text.Blaze                      as TB
import qualified Text.Blaze.Html5                as H
import           Text.Blaze.Html5                
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.Blaze.Html5.Attributes     
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Internal             as TBI

import           Text.Hamlet
import           Text.Lucius
import           Text.Cassius
import           Text.Julius

import           WebUI.Widgets.UIWidget
import           WebUI.Widgets.Components.UIBase



-- | Метод подключения расширения библиотеки jQuery (JQueryExtension)
jQueryExtension :: WidgetUI -> UI WidgetUI
jQueryExtension shell = do
    uuid <- generateUUID
    extension <- return $ createWidgetUI elem uuid "extension_jQuery"
    extension <- return $ extension { wui_scripts = (wui_scripts shell) ++ [H.script ! src "/jquery.js" $ ""] }
    shell `addWUI` extension
    where
        elem = EmptyElementUI 


