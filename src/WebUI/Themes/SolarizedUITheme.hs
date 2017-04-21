----------------------------------------------------------------
-- Модуль приложения
-- Графический интерфейс (UI)
-- Тема ГИП: Solarized 
----------------------------------------------------------------

module WebUI.Themes.SolarizedUITheme
    ( SolarizedTh (..)
    , thC_03
    , thC_02
    , thC_01
    , thC_00
    , thC_0 
    , thC_1 
    , thC_2 
    , thC_3 
    , thC_Yl
    , thC_Or
    , thC_Rd
    , thC_Mg
    , thC_Vl
    , thC_Bl
    , thC_Cn
    , thC_Gr
    , thC
    ) where

-- Импорт модулей
import           WebUI.Themes.UITheme 



-- Тип темы
data SolarizedTh = SolarizedTh

instance CUITheme SolarizedTh where
    isTheme _ = True

    rndTh SolarizedTh ThC_03    = "#002b36"
    rndTh SolarizedTh ThC_02    = "#073642"
    rndTh SolarizedTh ThC_01    = "#586e75"
    rndTh SolarizedTh ThC_00    = "#657b83"
    rndTh SolarizedTh ThC_0     = "#839496"
    rndTh SolarizedTh ThC_1     = "#93a1a1"
    rndTh SolarizedTh ThC_2     = "#eee8d5"
    rndTh SolarizedTh ThC_3     = "#fdf6e3"
    rndTh SolarizedTh ThC_Yl    = "#b58900"
    rndTh SolarizedTh ThC_Or    = "#cb4b16"
    rndTh SolarizedTh ThC_Mg    = "#dc322f"
    rndTh SolarizedTh ThC_Rd    = "#d33682"
    rndTh SolarizedTh ThC_Vl    = "#6c71c4"
    rndTh SolarizedTh ThC_Bl    = "#268bd2"
    rndTh SolarizedTh ThC_Cn    = "#2aa198"
    rndTh SolarizedTh ThC_Gr    = "#859900"
    rndTh SolarizedTh (ThC txt) = "#" ++ txt


-- | Текущая тема UI
currentTheme = SolarizedTh

rndThCur = rndTh currentTheme



-- | Цвета
thC_03  = rndThCur ThC_03
thC_02  = rndThCur ThC_02
thC_01  = rndThCur ThC_01
thC_00  = rndThCur ThC_00
thC_0   = rndThCur ThC_0
thC_1   = rndThCur ThC_1
thC_2   = rndThCur ThC_2
thC_3   = rndThCur ThC_3
thC_Yl  = rndThCur ThC_Yl
thC_Or  = rndThCur ThC_Or
thC_Rd  = rndThCur ThC_Rd
thC_Mg  = rndThCur ThC_Mg
thC_Vl  = rndThCur ThC_Vl
thC_Bl  = rndThCur ThC_Bl
thC_Cn  = rndThCur ThC_Cn
thC_Gr  = rndThCur ThC_Gr
thC     = rndThCur . ThC 

