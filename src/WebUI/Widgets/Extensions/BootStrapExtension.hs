----------------------------------------------------------------
-- Модуль приложения
-- Расширение графического интерфейса (ExtensionUI)
-- Инструментарий Bootstrap
----------------------------------------------------------------

module WebUI.Widgets.Extensions.BootStrapExtension
    ( bootStrapExtension

    , bootStrap_Btn

    , bootStrap_BtnDefault
    , bootStrap_BtnPrimary
    , bootStrap_BtnSuccess
    , bootStrap_BtnInfo 
    , bootStrap_BtnWarning
    , bootStrap_BtnDanger 
    , bootStrap_BtnLink 

    , bootStrap_BtnLG
    , bootStrap_BtnMD
    , bootStrap_BtnSM
    , bootStrap_BtnXS

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



-- | Метод подключения расширения всплывающей подсказки (ToolTipExtension)
bootStrapExtension :: WidgetUI -> UI WidgetUI
bootStrapExtension shell = do
    uuid <- generateUUID
    extension <- return $ createWidgetUI elem uuid "extension_ToolTipExtension"
    extension <- return $ extension { wui_scripts = (wui_scripts shell) ++ [ H.script ! src "/bootstrap.js" $ ""
                                                          , H.link ! rel "stylesheet" ! type_ "text/css" ! href "/bootstrap.css" 
                                                          , H.link ! rel "stylesheet" ! type_ "text/css" ! href "/bootstrap-theme.css" 
                                                          , H.link ! rel "stylesheet" ! type_ "text/css" ! href "/bootstrap-solarized-dark.css" 
                                                          ] 
                                    }
    shell `addWUI` extension
    where
        elem = EmptyElementUI 


bootStrap_Btn :: WidgetUI -> UI WidgetUI
bootStrap_Btn = classToWidget "btn"



bootStrap_BtnDefault :: WidgetUI -> UI WidgetUI
bootStrap_BtnDefault = classToWidget "btn-default"

bootStrap_BtnPrimary :: WidgetUI -> UI WidgetUI
bootStrap_BtnPrimary = classToWidget "btn-primary"

bootStrap_BtnSuccess :: WidgetUI -> UI WidgetUI
bootStrap_BtnSuccess = classToWidget "btn-success"

bootStrap_BtnInfo :: WidgetUI -> UI WidgetUI
bootStrap_BtnInfo = classToWidget "btn-info"

bootStrap_BtnWarning :: WidgetUI -> UI WidgetUI
bootStrap_BtnWarning = classToWidget "btn-warning"

bootStrap_BtnDanger :: WidgetUI -> UI WidgetUI
bootStrap_BtnDanger = classToWidget "btn-danger"

bootStrap_BtnLink :: WidgetUI -> UI WidgetUI
bootStrap_BtnLink = classToWidget "btn-link"


 
bootStrap_BtnLG :: WidgetUI -> UI WidgetUI
bootStrap_BtnLG = classToWidget "btn-lg"

bootStrap_BtnMD :: WidgetUI -> UI WidgetUI
bootStrap_BtnMD = classToWidget "btn-md"

bootStrap_BtnSM :: WidgetUI -> UI WidgetUI
bootStrap_BtnSM = classToWidget "btn-sm"

bootStrap_BtnXS :: WidgetUI -> UI WidgetUI
bootStrap_BtnXS = classToWidget "btn-xs"



