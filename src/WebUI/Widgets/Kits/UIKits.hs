----------------------------------------------------------------
-- Модуль приложения
-- Графический интерфейс (UI)
-- Наборы Kits 
----------------------------------------------------------------

module WebUI.Widgets.Kits.UIKits
    ( kitContCenterFlex, kitContCenterFlex_ 
    ) where

-- Импорт модулей

import           Prelude                         as PRL

import           WebUI.Widgets.UIWidget



-- | Набор для центрирования содержимого по горизонтали и вертикали
kitContCenterFlex :: WidgetUI -> UI WidgetUI
kitContCenterFlex widget = 
    (return widget) <#> flexD
                    <#> styleToWidget "align-items: center; justify-content: center;"
        
kitContCenterFlex_ = unsafePerformUI . kitContCenterFlex



