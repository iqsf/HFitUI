----------------------------------------------------------------
-- Модуль приложения
-- Графический интерфейс (UI)
-- Тема ГИП (Типы) 
----------------------------------------------------------------

module WebUI.Themes.UIThemeTypes
    ( ThParam       (..)
    , CUIThemeParam (..)
    , CUITheme      (..)
    ) where

-- Импорт модулей



-- Тип параметров темы
data ThParam =              -- | Фиксированный набор цветов
               ThC_03       -- | base 03 
             | ThC_02       -- | base 02
             | ThC_01       -- | base 01
             | ThC_00       -- | base 00
             | ThC_0        -- | base 0
             | ThC_1        -- | base 1
             | ThC_2        -- | base 2
             | ThC_3        -- | base 3
             | ThC_Yl       -- | yellow
             | ThC_Or       -- | orange
             | ThC_Rd       -- | red
             | ThC_Mg       -- | magenta
             | ThC_Vl       -- | violet
             | ThC_Bl       -- | blue
             | ThC_Cn       -- | cyan
             | ThC_Gr       -- | green
             | ThC String   -- | Другой цвет по имени
                            -- | Фиксированный набор шрифтов
             | ThF_03       -- | Font 03
             | ThF_02       -- | Font 02
             | ThF_01       -- | Font 01
             | ThF_0        -- | Font 0
             | ThF_1        -- | Font 1
             | ThF_2        -- | Font 2
             | ThF_3        -- | Font 3
             | ThF String   -- | Другой шрифт по имени

             | ThPrm String -- | Параметр общий



-- Класс типа параметра темы 
class CUIThemeParam b where
    isThemeParam :: b -> Bool

instance CUIThemeParam ThParam where
    isThemeParam _ = True



-- Класс темы
class CUITheme a where
    isTheme :: a -> Bool
    rndTh   :: a -> ThParam -> String



