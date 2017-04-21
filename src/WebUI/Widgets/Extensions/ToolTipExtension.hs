----------------------------------------------------------------
-- Модуль приложения
-- Расширение графического интерфейса (ExtensionUI)
-- Всплывающая подсказка
----------------------------------------------------------------

module WebUI.Widgets.Extensions.ToolTipExtension
    ( toolTipExtension

    , extToolTip
    , extToolTipW
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



-- | Подготовка CSS
prepareCSS a = toHtml $ renderCssUrl undefined a



-- http://www.w3schools.com/howto/howto_css_tooltip.asp

-- | Метод подключения расширения всплывающей подсказки (ToolTipExtension)
toolTipExtension :: WidgetUI -> UI WidgetUI
toolTipExtension shell = do
    uuid <- generateUUID
    extension <- return $ createWidgetUI elem uuid "extension_ToolTipExtension"
    extension <- return $ extension { wui_styles = (wui_styles extension) ++ [H.style $ do 
        prepareCSS ([cassius|
            .tooltip 
                position: relative
                display: inline-block
            
            .tooltip .tooltiptext 
                visibility: hidden
                width: auto
                min-width: 120px
                background-color: #555
                color: #fff
                text-align: center
                border-radius: 6px
                padding: 5px 0
                position: absolute
                z-index: 100000
                bottom: 125%
                left: 50%
                margin-left: -60px
                opacity: 0
                transition: opacity 1s


            .tooltip .tooltiptext::after 
                content: ""
                position: absolute
                top: 100%
                left: 50%
                margin-left: -5px
                border-width: 5px
                border-style: solid
                border-color: #555 transparent transparent transparent

            .tooltip:hover .tooltiptext 
                visibility: visible
                opacity: 1
        |] )] }
    --extension <- return $ extension { wui_styles = (wui_styles extension) ++ [H.style ".test_qwerty {position: display;}"] }
    shell `addWUI` extension
    where
        elem = EmptyElementUI 



-- | Добавление всплывающей подсказки с текстом
extToolTip :: String -> WidgetUI -> UI WidgetUI
extToolTip val widget = do 
    case (wui_element widget) of
            ContainerUI f   -> do widget <- classToWidget "tooltip" widget 
                                            `addWUImms` 
                                            [textSpan val <#> classToWidget "tooltiptext"]
                                  return widget
            ElementUI e     -> do return widget
            EmptyElementUI  -> do return widget



-- | Добавление всплывающей подсказки с виджетом
extToolTipW :: WidgetUI -> WidgetUI -> UI WidgetUI
extToolTipW valw widget = do 
    case (wui_element widget) of
            ContainerUI f   -> do widget <- classToWidget "tooltip" widget 
                                            `addWUImms` 
                                            [classToWidget "tooltiptext" valw]
                                  return widget
            ElementUI e     -> do return widget
            EmptyElementUI  -> do return widget



