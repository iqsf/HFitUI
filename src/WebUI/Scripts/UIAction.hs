----------------------------------------------------------------
-- Модуль приложения
-- Графический интерфейс (UI)
-- Действия 
----------------------------------------------------------------

module WebUI.Scripts.UIAction
    ( onClick   , onClick_
    , onRedirect, onRedirect_
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

import           WebUI.Widgets.UIWidget
import           WebUI.Themes.SolarizedUITheme    



----------------------------------------------------------------
-- ДЕЙСТВИЯ   --------------------------------------------------
----------------------------------------------------------------

-- | Отработка onclick 
onClick :: String -> WidgetUI -> UI WidgetUI
onClick textScript widget = do
    case wui_element widget of
        EmptyElementUI  -> return widget
        ElementUI a     -> return widget { wui_element = ElementUI   (a ! HA.onclick (toAttrVal textScript)) }
        ContainerUI b   -> return widget { wui_element = ContainerUI (b ! HA.onclick (toAttrVal textScript)) }

onClick_ s w = unsafePerformUI $ onClick s w
{-# DEPRECATED onClick_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Отработка перенаправления на другой URL
onRedirect :: String -> WidgetUI -> UI WidgetUI
onRedirect textURL widget = onClick ("location.href = '" ++ textURL ++ "';") widget

onRedirect_ s w = unsafePerformUI $ onRedirect s w
{-# DEPRECATED onRedirect_ "This is unsafe! It is not recommended to use this function!" #-}



