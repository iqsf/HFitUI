----------------------------------------------------------------
-- Модуль приложения
-- Расширение графического интерфейса (ExtensionUI)
-- Карта OSM
----------------------------------------------------------------

module WebUI.Widgets.Extensions.MapOSMExtension
    ( mapOSMExtension
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



-- | Метод подключения расширения карты OSM (MapOSMExtension)
mapOSMExtension :: WidgetUI -> UI WidgetUI
mapOSMExtension shell = do
    uuid <- generateUUID
    extension <- return $ createWidgetUI elem uuid "extension_MapOSM"
    extension <- return $ extension { wui_scripts = (wui_scripts shell) ++ 
                                                          [ H.script ! src "/ol.js" $ ""
                                                          , H.link ! rel "stylesheet" ! type_ "text/css" ! href "/ol.css" 
                                                          ] 
                                    }
    shell `addWUI` extension
    where
        elem = EmptyElementUI 


