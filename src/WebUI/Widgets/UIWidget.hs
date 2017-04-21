----------------------------------------------------------------
-- Модуль приложения
-- Графический интерфейс (UI)
-- Виджет ГИП 
----------------------------------------------------------------

module WebUI.Widgets.UIWidget
    ( UI (..)
    , AdaptableUI
    , UIColor (..)
    , UIPosition (..)
    , UIImage (..)
    , WidgetUI (..) 
    , StructureTypeUI (..) 
    , ElementUI (..) 
--    , MonadUI (..)

    , CArgumentableWidget
    , pack

    , (<#>)
    , (<~>)
    , (##)
    , liftUI
    , unsafePerformUI
    , packWidgetUI
    , expandWUI
    , makeBaseSTypeUI, makeAggregativeSTypeUI
    , generateUUID, generateUUID_
    , toAttrVal
    , generateValueUUID
    , createWidgetUI
    , requare

    , absolute, fixed, relative, static, inherit

    , block, inline, inline_block, inline_table, list_item, none, run_in, tableD, table_caption, table_cell 
    , table_column_group, table_column, table_footer_group, table_header_group, table_row, table_row_group
    , flexD

    , cursorAuto, cursorCrosshair, cursorDefault, cursorEResize, cursorHelp, cursorMove, cursorNResize, cursorNEResize, cursorNWResize 
    , cursorPointer, cursorProgress, cursorSResize, cursorSEResize, cursorSWResize, cursorText, cursorWResize, cursorWait, cursorInherit 
    , cursorImage

    , overflowAuto , overflowHidden , overflowScroll , overflowVisible , overflowInherit
    , overflowAutoX, overflowHiddenX, overflowScrollX, overflowVisibleX, overflowInheritX
    , overflowAutoY, overflowHiddenY, overflowScrollY, overflowVisibleY, overflowInheritY

    , BGRepeat (..), BGAttachment (..), BGSize (..)
    , background, backgroundColor, backgroundRepeat, backgroundAttachment, backgroundImage, backgroundSize

    , BorderStyle (..)
    , border, border_left, border_right, border_top, border_bottom
    , borderNone, border_leftNone, border_rightNone, border_topNone, border_bottomNone
    , borderRadius, borderRadiusNum, borderRadiusStr, borderRadiusExt

    , UI_StartingPoint (..), HSize (..)
    , bounds , boundsNum , boundsStr , boundsExt , boundsBRD , boundsBRDNum , boundsBRDStr , boundsBRDExt

    , margin, marginNum, marginStr, marginExt, marginAll, marginAllNum, marginAllStr, marginAllExt

    , marginLeft  , marginLeftNum  , marginLeftStr  , marginLeftExt   
    , marginRight , marginRightNum , marginRightStr , marginRightExt  
    , marginTop   , marginTopNum   , marginTopStr   , marginTopExt    
    , marginBottom, marginBottomNum, marginBottomStr, marginBottomExt 

    , padding, paddingNum, paddingStr, paddingExt, paddingAll, paddingAllNum, paddingAllStr, paddingAllExt

    , paddingLeft  , paddingLeftNum  , paddingLeftStr  , paddingLeftExt   
    , paddingRight , paddingRightNum , paddingRightStr , paddingRightExt  
    , paddingTop   , paddingTopNum   , paddingTopStr   , paddingTopExt    
    , paddingBottom, paddingBottomNum, paddingBottomStr, paddingBottomExt 

    , wwidth    , widthNum    , widthStr    , widthExt
    , wwidthMin , widthMinNum , widthMinStr , widthMinExt
    , wwidthMax , widthMaxNum , widthMaxStr , widthMaxExt
    
    , wheight   , heightNum   , heightStr   , heightExt
    , wheightMin, heightMinNum, heightMinStr, heightMinExt
    , wheightMax, heightMaxNum, heightMaxStr, heightMaxExt
    
    , wsize  , sizeNum  , sizeStr  , sizeExt

    , left  , leftNum  , leftStr  , leftExt
    , right , rightNum , rightStr , rightExt
    , top   , topNum   , topStr   , topExt
    , bottom, bottomNum, bottomStr, bottomExt

    , foreground

    , fill

    , FFamily (..), FSize (..), FontStyle (..), FontWeight (..) 
    , UIFont (..)
    , ffSerif, ffSansSerif, ffCursive, ffFantasy, ffMonospace
    , font, fontFamily, fontSize, fontStyle, fontWeight

    , zIndex

    , classToWidget 
    , styleToWidget

    , tooltip 
 
    , hover, hoverm

    , addWUI, addWUI_, addWUIs, addWUIs_, addWUIm, addWUImm, addWUIsm, addWUIms, addWUImms
    , addWUIcf, addWUIcfs

    , DecoratorUI (..)
    , forLoopUI

    , shellUI, shellExtUI
    , wuiElement, wuiElement_
    , wuiHTML, wuiHTML_
    , wuiScriptSrc
    , wuiStyleHref
    , wuiScriptJS, wuiScriptJSs
    , wuiScriptTextJS, wuiScriptTextJSs
    , wuiCSS, wuiCSSs
    ) where

-- Импорт модулей
--import           Import 

import           Prelude                         as PRL
import           Data.Maybe                      (fromJust)
import           System.IO
import           Control.Monad.IO.Class
--import Control.Monad (MonadPlus(mzero, mplus), liftM, ap)

import qualified Text.Blaze                      as TB
import qualified Text.Blaze.Html5                as H
import           Text.Blaze.Html5                
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.Blaze.Html5.Attributes     
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Internal             as TBI

import           System.IO.Unsafe             (unsafePerformIO)
import           System.Process
--import           System.Exit

import           Data.UUID                       as UV
import           Data.UUID.V1                    as UV1
import           Data.UUID.V4                    as UV4
import           Data.UUID.V5                    as UV5
import           System.Random
import           Text.Hamlet
import           Text.Lucius
import           Text.Cassius
import           Text.Julius

import           WebUI.Themes.UITheme
import           WebUI.Themes.SolarizedUITheme


--type WUI a = StateT s m a
--type WUI a = StateT SettingsUI IO a

--data UI a = UI { unUI :: a } 
--             deriving Functor


--instance Applicative UI where
--    pure   = UI
--    m *>  k = m >>= \ _ -> k
--    m <*  k = m >>= \ _ -> m
--    m <*> k = UI $ (unUI m) (unUI k)


--instance Monad UI where
--    m >>  k = m >>= \ _ -> k
--    m >>= k = k $ unUI m
--    return  = UI


--data SettingsUI = SettingsUI { uiLocal :: String }


--instance MonadIO UI where
--    liftIO a = return $ memo $ unsafePerformIO a

--bindUI :: UI a -> (a -> UI b) -> UI b
--bindUI (UI m) k = UI (\ s -> case m s of (# new_s, a #) -> unUI (k a) new_s)

-- | Тип UI
type UI = IO


-- | Общий класс всех типов адаптированных для работы в UI
class AdaptableUI a where
    isAdapterUI :: a -> Bool


--class (Monad m) => MonadUI m where
--    -- | Lift a computation from the 'IO' monad.
--    liftUI :: UI a -> m a


    
--instance MonadUI UI where
--    liftUI = PRL.id    


liftUI a = liftIO a

--unsafePerformUI = unUI
unsafePerformUI = unsafePerformIO
{-# DEPRECATED unsafePerformUI "This is unsafe! It is not recommended to use this function!" #-}


-- | Тип цвета
type UIColor = String

-- | Тип положения
data UIPosition = UILeft | UIRight | UITop | UIBottom | UICenter | UIInherit
instance Show UIPosition where
    show UILeft    = "left"
    show UIRight   = "right"
    show UITop     = "top"
    show UIBottom  = "bottom"
    show UICenter  = "center"
    show UIInherit = "inherit"

-- | Давнные изображения
data UIImage = ImageEmpty
             | ImageURL String
             | ImageBase64 String String

instance Show UIImage where
    show ImageEmpty        = "";
    show (ImageURL url)    = "url('" ++ url ++ "')"
    show (ImageBase64 t b) = "url('data:image/" ++ t ++ ";base64," ++ b ++ "')"


-- | Тип структуры UI
data StructureTypeUI = BaseSTypeUI
                     | AggregativeSTypeUI
                     deriving Show



-- | Элемент UI
data ElementUI = EmptyElementUI 
               | ElementUI H.Html
               | ContainerUI (H.Html -> H.Html)


instance Show ElementUI where
    show EmptyElementUI           = "EmptyElementUI"
    show (ElementUI a)            = "EmptyUI (" ++ (show a) ++ ")"
    show _                        = "ContainerUI (" ++ "H.Html -> H.Html"  ++ ")"
    --show (ContainerUI (a -> b))   = "(" ++ (show a) ++ ")"

instance Show (ElementUI -> ElementUI) where
    show _           = "Builder"


-- | Данные виджета UI
data WidgetUI = EmptyUI                                                  -- ^ Пустой UI
              | WidgetUI { wui_title      :: H.Html                      -- ^ Заголовок страницы
                         , wui_id         :: String                      -- ^ ID виджета
                         , wui_ui_name    :: String                      -- ^ Наименование комопонента виджета
                         , wui_attr_style :: String                      -- ^ Атрибут стилей
                         , wui_attr_class :: String                      -- ^ Атрибут классов
                         , wui_styles     :: [H.Html]                    -- ^ Массив стилей для HEAD раздела страницы
                         , wui_scripts    :: [H.Html]                    -- ^ Массив скриптов для HEAD раздела страницы
                         , wui_contents   :: [H.Html]                    -- ^ Массив содержимого для BODY раздела страницы (глобальный)
                         , wui_children   :: [WidgetUI]                  -- ^ Дочерние виджеты
                         , wui_strTypeUI  :: StructureTypeUI             -- ^ Тип структуры UI виджета
                         , wui_element    :: ElementUI                   -- ^ Элемент UI виджета
                         , wui_preBuild   :: (ElementUI -> ElementUI)    -- ^ Предварительный строитель
                         , wui_postBuild  :: (ElementUI -> ElementUI)    -- ^ Заключительный строитель
                         } deriving Show


instance Show H.Html where
    show = renderHtml 

instance Show AttributeValue where
    show _  = "AttributeValue" 


-- | Значение по умолчанию данных виджета UI
defaultWidgetUI :: UI WidgetUI
defaultWidgetUI = do 
    uuid <- generateUUID
    return $ WidgetUI { wui_title      = ""              -- ^ Заголовок страницы
                      , wui_id         = uuid            -- ^ ID виджета
                      , wui_ui_name    = "shell"         -- ^ Наименование компонента виджета
                      , wui_attr_style = ""              -- ^ Атрибут стилей
                      , wui_attr_class = ""              -- ^ Атрибут классов
                      , wui_styles     = []              -- ^ Массив стилей для HEAD раздела страницы
                      , wui_scripts    = []              -- ^ Массив скриптов для HEAD раздела страницы
                      , wui_contents   = []              -- ^ Массив содержимого для BODY раздела страницы (глобальный)
                      , wui_children   = []              -- ^ Дочерние виджеты
                      , wui_strTypeUI  = BaseSTypeUI     -- ^ Тип структуры UI виджета
                      , wui_element    = EmptyElementUI  -- ^ Элемент UI виджета
                      , wui_preBuild   = (\x -> x)       -- ^ Предварительный строитель
                      , wui_postBuild  = (\x -> x)       -- ^ Заключительный строитель
                      }



-- | Создать данные виджета UI
createWidgetUI :: ElementUI -> String -> String -> WidgetUI
createWidgetUI element uuid ui_name = 
    WidgetUI { wui_title      = ""                            -- ^ Заголовок страницы
             , wui_id         = uuid                          -- ^ ID виджета
             , wui_ui_name    = ui_name                       -- ^ Наименование компонента виджета
             , wui_attr_style = ""                            -- ^ Атрибут стилей
             , wui_attr_class = ""                            -- ^ Атрибут классов
             , wui_styles     = []                            -- ^ Массив стилей для HEAD раздела страницы
             , wui_scripts    = []                            -- ^ Массив скриптов для HEAD раздела страницы
             , wui_contents   = []                            -- ^ Массив содержимого для BODY раздела страницы (глобальный)
             , wui_children   = []                            -- ^ Дочерние виджеты
             , wui_strTypeUI  = BaseSTypeUI                   -- ^ Тип структуры UI виджета
             , wui_element    = element                       -- ^ Элемент UI виджета
             , wui_preBuild   = (\x -> x)                     -- ^ Предварительный строитель
             , wui_postBuild  = (\x -> x)                     -- ^ Заключительный строитель
             }



-- | Сделать структурный тип виджета Base
makeBaseSTypeUI :: WidgetUI 
                -> UI WidgetUI
makeBaseSTypeUI widget = do
    return $ widget {wui_strTypeUI = BaseSTypeUI}



-- | Сделать структурный тип виджета Aggregative
makeAggregativeSTypeUI :: WidgetUI 
                       -> UI WidgetUI
makeAggregativeSTypeUI widget = do
    return $ widget {wui_strTypeUI = AggregativeSTypeUI}



-- | Сгенерировать UUID
--generateUUID :: UI String
--generateUUID = do
--    muuid <- UV1.nextUUID 
--    case muuid of
--        Just uuid   -> do return $ UV.toString uuid 
--        Nothing     -> do ruuid <- UV4.nextRandom
--                          return $ UV.toString ruuid

--generateUUID :: IO String
--generateUUID = UV.toString . fromJust <$> UV1.nextUUID

generateUUID :: UI String
generateUUID = do 
    ruuid <- UV4.nextRandom
--    ruuid <- liftIO $ UV4.nextRandom
    return $ UV.toString ruuid

generateUUID_ = unsafePerformUI generateUUID
{-# DEPRECATED generateUUID_ "This is unsafe! It is not recommended to use this function!" #-}

-- | Перевести в значение AttributeValue
toAttrVal = TB.toValue




-- | Сгенерировать UUID как значение AttributeValue
generateValueUUID :: AttributeValue
generateValueUUID = TB.toValue generateValueUUID



-- | Применить виджет к виджету
appendUI :: WidgetUI -> WidgetUI -> UI WidgetUI
appendUI w_1 w_2 = do
    return $ w_1 { wui_children = (wui_children w_1) ++ [w_2] }



-- | Добавить виджет к виджету
addWUI :: WidgetUI -> WidgetUI -> UI WidgetUI
addWUI w_1 w_2 = do
    case (wui_strTypeUI w_1) of
        BaseSTypeUI         -> return $ w_1 { wui_children = (wui_children w_1) ++ [w_2] }
        AggregativeSTypeUI  -> addWUIcf w_1 w_2

addWUI_ w_1 w_2 = unsafePerformUI $ addWUI w_1 w_2
{-# DEPRECATED addWUI_ "This is unsafe! It is not recommended to use this function!" #-}


-- | Добавить виджет к виджету (виджет в монаде UI)
addWUIm :: WidgetUI -> UI WidgetUI -> UI WidgetUI
addWUIm w_1 w_2 = do
    a2 <- w_2
    addWUI w_1 a2


-- | Добавить виджет к виджету (виджеты в монаде UI)
addWUImm :: UI WidgetUI -> UI WidgetUI -> UI WidgetUI
addWUImm w_1 w_2 = do
    a1 <- w_1
    a2 <- w_2
    addWUI a1 a2


-- | Добавить виджеты к виджету
addWUIs :: WidgetUI -> [WidgetUI] -> UI WidgetUI
addWUIs p [] = do return p
addWUIs p (x:xs) = do 
    result <- addWUI p x
    addWUIs result xs

addWUIs_ w_1 w_2 = unsafePerformUI $ addWUIs w_1 w_2
{-# DEPRECATED addWUIs_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Добавить виджеты к виджету (виджеты в монаде UI)
addWUIsm :: UI WidgetUI -> UI [WidgetUI] -> UI WidgetUI
addWUIsm w_1 w_2 = do 
    a1 <- w_1
    a2 <- w_2
    addWUIs a1 a2 



-- | Добавить виджеты к виджету (массив виджетов в монаде UI)
addWUIms :: WidgetUI -> [UI WidgetUI] -> UI WidgetUI
addWUIms w_1 [] = do return w_1
addWUIms w_1 (x:xs) = do 
    a <- x
    r <- addWUI w_1 a 
    addWUIms r xs



-- | Добавить виджеты к виджету (виджет и массив виджетов в монаде UI)
addWUImms :: UI WidgetUI -> [UI WidgetUI] -> UI WidgetUI
addWUImms w_1 ws = do 
    w <- w_1
    addWUIms w ws



-- | Добавить виджет к первому дочерниму виджету виджета
addWUIcf :: WidgetUI -> WidgetUI -> UI WidgetUI
addWUIcf w_1 w_2 = do
    chl <- return $ wui_children w_1
    case chl of
        []      -> appendUI w_1 w_2
        (x:xs)  -> do cf <- x `addWUI` w_2
                      return w_1 { wui_children = [cf] ++ xs }


-- | Добавить виджеты к первому дочерниму виджету виджета
addWUIcfs :: WidgetUI -> [WidgetUI] -> UI WidgetUI
addWUIcfs p [] = do return p
addWUIcfs p (x:xs) = do 
    result <- addWUIcf p x
    addWUIs result xs



-- | Тип декоратора UI
type DecoratorUI a = WidgetUI -> a -> UI WidgetUI

-- | Цикл петли прохода по списку в UI
forLoopUI :: WidgetUI       -- ^ Родительский виджет
          -> [a]            -- ^ Список
          -> DecoratorUI a  -- ^ Декоратор
          -> UI WidgetUI    -- ^ Результат
forLoopUI parent []     _      = do 
    return parent
forLoopUI parent (x:xs) action = do
    parentNext <- action parent x
    forLoopUI parentNext xs action



-- | Класс для реализации заполнения частично примененных атрибутов
class CArgumentableWidget a where
    pack :: a -> UI WidgetUI

instance CArgumentableWidget (UI WidgetUI) where pack a = a  



-- | Класс для реализации заполнения частично примененных атрибутов
class CArgumentableAttr a where
    expressArgumentAttr :: a -> (WidgetUI -> UI WidgetUI)

instance CArgumentableAttr (WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a  



-- | Применить атрибуты через адаптеры функции
--(<#>) :: UI WidgetUI -> (WidgetUI -> UI WidgetUI) -> UI WidgetUI
--(<#>) w wfn  = do {a <- w; wfn a; }
(<#>) :: CArgumentableAttr a => UI WidgetUI -> a -> UI WidgetUI
(<#>) w wfn = do 
    a <- w 
    (expressArgumentAttr wfn) a
infixl 9 <#>


(<~>) :: CArgumentableAttr a => WidgetUI -> a -> WidgetUI
{-# DEPRECATED (<~>) "This is unsafe! It is not recommended to use this function!" #-}
(<~>) w wfn = do 
    unsafePerformUI $ (expressArgumentAttr wfn) w
infixl 9 <~>


-- | Применить аттрибуты Blaze
(##) :: UI WidgetUI -> Attribute -> UI WidgetUI
(##) a atr  = do
    w <- a
    element <- return $ wui_element w
    case element of
        ContainerUI f   -> do return w { wui_element = ContainerUI (f ! atr) }
        ElementUI e     -> do return w { wui_element = ElementUI (e ! atr) }
        EmptyElementUI  -> do return w
        --otherwise       -> do return w { wui_element = element ! atr }
infixl 9 ##



-- | Упаковать виджет для представления в Blaze
--   с применением атрибутов
packWidgetUI :: WidgetUI -> WidgetUI
packWidgetUI widget =
    case (wui_element widget) of
            ContainerUI f   -> widget { wui_element = a_postBuild . ContainerUI $ f ! HA.id a_uuid 
                                                                                    ! TBI.customAttribute "ui_widget" a_ui_name 
                                                                                    ! HA.style a_styles 
                                                                                    ! HA.class_ a_classes
                                      }
            ElementUI e     -> widget { wui_element = a_postBuild . ElementUI   $ e ! HA.id a_uuid 
                                                                                    ! TBI.customAttribute "ui_widget" a_ui_name 
                                                                                    ! HA.style a_styles 
                                                                                    ! HA.class_ a_classes
                                      }
            EmptyElementUI  -> widget
    where
        a_uuid      = toAttrVal . wui_id         $ widget
        a_ui_name   = toAttrVal . wui_ui_name    $ widget
        a_styles    = toAttrVal . wui_attr_style $ widget
        a_classes   = toAttrVal . wui_attr_class $ widget
        a_preBuild  =             wui_preBuild   $ widget
        a_postBuild =             wui_postBuild  $ widget



-- | Разобрать элемент
expandWUI :: UI WidgetUI 
          -> UI (WidgetUI, String, String)
expandWUI widget = do
    w <- widget
    return (w, (wui_id w), (wui_ui_name w))



-- | Подключение расширения
requare :: WidgetUI -> (WidgetUI -> UI WidgetUI) -> UI WidgetUI
requare shell extensionDo = do
    extensionDo shell



-----------------------------------------------------------------------------------------------
-- Адаптеры атрибутов   -----------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- | Способ позиционирования элемента absolute
absolute :: WidgetUI -> UI WidgetUI
absolute widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "position: absolute;" }
 
-- | Способ позиционирования элемента fixed
fixed :: WidgetUI -> UI WidgetUI
fixed widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "position: fixed;" }

-- | Способ позиционирования элемента relative
relative :: WidgetUI -> UI WidgetUI
relative widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "position: relative;" }

-- | Способ позиционирования элемента static
static :: WidgetUI -> UI WidgetUI
static widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "position: static;" }

-- | Способ позиционирования элемента inherit
inherit :: WidgetUI -> UI WidgetUI
inherit widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "position: inherit;" }



-- | Способ отображения block
block :: WidgetUI -> UI WidgetUI
block widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: block;" }

-- | Способ отображения inline 
inline :: WidgetUI -> UI WidgetUI
inline widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: inline;" }

-- | Способ отображения inline_block
inline_block :: WidgetUI -> UI WidgetUI
inline_block widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: inline-block;" }

-- | Способ отображения inline_table
inline_table :: WidgetUI -> UI WidgetUI
inline_table widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: inline-table;" }

-- | Способ отображения list_item
list_item :: WidgetUI -> UI WidgetUI 
list_item widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: list-item;" }

-- | Способ отображения none
none :: WidgetUI -> UI WidgetUI
none widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: none;" }

-- | Способ отображения run_in
run_in :: WidgetUI -> UI WidgetUI
run_in widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: run-in;" }

-- | Способ отображения table
tableD :: WidgetUI -> UI WidgetUI
tableD widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: table;" }

-- | Способ отображения table_caption
table_caption :: WidgetUI -> UI WidgetUI
table_caption widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: table-caption;" }

-- | Способ отображения table_cell
table_cell :: WidgetUI -> UI WidgetUI
table_cell widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: table-cell;" }

-- | Способ отображения 
table_column_group :: WidgetUI -> UI WidgetUI
table_column_group widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: table-column-group;" }

-- | Способ отображения table_column
table_column :: WidgetUI -> UI WidgetUI
table_column widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: table-column;" }

-- | Способ отображения table_footer_group
table_footer_group :: WidgetUI -> UI WidgetUI
table_footer_group widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: table-footer-group;" }

-- | Способ отображения table_header_group
table_header_group :: WidgetUI -> UI WidgetUI
table_header_group widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: table-header-group;" }

-- | Способ отображения table_row
table_row :: WidgetUI -> UI WidgetUI
table_row widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: table-row;" }

-- | Способ отображения table_row_group
table_row_group :: WidgetUI -> UI WidgetUI
table_row_group widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: table-row-group;" }

-- | Способ отображения flex
flexD :: WidgetUI -> UI WidgetUI
flexD widget = do return widget { wui_attr_style = (wui_attr_style widget) ++ "display: flex;" }



-- | Курсор auto
cursorAuto :: WidgetUI -> UI WidgetUI
cursorAuto widget = applyStyleToWidget widget "cursor: auto;"

-- | Курсор auto
cursorCrosshair :: WidgetUI -> UI WidgetUI
cursorCrosshair widget = applyStyleToWidget widget "cursor: crosshair;"

-- | Курсор default
cursorDefault :: WidgetUI -> UI WidgetUI
cursorDefault  widget = applyStyleToWidget widget "cursor: default;"

-- | Курсор e-resize
cursorEResize :: WidgetUI -> UI WidgetUI
cursorEResize  widget = applyStyleToWidget widget "cursor: e-resize;"

-- | Курсор help
cursorHelp :: WidgetUI -> UI WidgetUI
cursorHelp widget = applyStyleToWidget widget "cursor: help;"

-- | Курсор move
cursorMove :: WidgetUI -> UI WidgetUI
cursorMove widget = applyStyleToWidget widget "cursor: move;"

-- | Курсор n-resize
cursorNResize :: WidgetUI -> UI WidgetUI
cursorNResize widget = applyStyleToWidget widget "cursor: n-resize;"

-- | Курсор ne-resize
cursorNEResize :: WidgetUI -> UI WidgetUI
cursorNEResize widget = applyStyleToWidget widget "cursor: ne-resize;"

-- | Курсор nw-resize
cursorNWResize :: WidgetUI -> UI WidgetUI
cursorNWResize widget = applyStyleToWidget widget "cursor: nw-resize;"

-- | Курсор pointer
cursorPointer :: WidgetUI -> UI WidgetUI
cursorPointer widget = applyStyleToWidget widget "cursor: pointer;"

-- | Курсор progress
cursorProgress :: WidgetUI -> UI WidgetUI
cursorProgress widget = applyStyleToWidget widget "cursor: progress;"

-- | Курсор s-resize
cursorSResize :: WidgetUI -> UI WidgetUI
cursorSResize widget = applyStyleToWidget widget "cursor: s-resize;"

-- | Курсор se-resize
cursorSEResize :: WidgetUI -> UI WidgetUI
cursorSEResize widget = applyStyleToWidget widget "cursor: se-resize;"

-- | Курсор sw-resize
cursorSWResize :: WidgetUI -> UI WidgetUI
cursorSWResize widget = applyStyleToWidget widget "cursor: sw-resize;"

-- | Курсор text
cursorText :: WidgetUI -> UI WidgetUI
cursorText widget = applyStyleToWidget widget "cursor: text;"

-- | Курсор w-resize
cursorWResize :: WidgetUI -> UI WidgetUI
cursorWResize widget = applyStyleToWidget widget "cursor: w-resize;"

-- | Курсор wait
cursorWait :: WidgetUI -> UI WidgetUI
cursorWait widget = applyStyleToWidget widget "cursor: wait;"

-- | Курсор auto
cursorInherit :: WidgetUI -> UI WidgetUI
cursorInherit widget = applyStyleToWidget widget "cursor: inherit;"

-- | Курсор по изображению
cursorImage :: UIImage -> WidgetUI -> UI WidgetUI
cursorImage image widget = applyStyleToWidget widget ("cursor: " ++ (show image) ++ ";")



-- | Отображение содержимого auto
overflowAuto :: WidgetUI -> UI WidgetUI
overflowAuto widget = applyStyleToWidget widget "overflow: auto;"

-- | Отображение содержимого hidden
overflowHidden :: WidgetUI -> UI WidgetUI
overflowHidden widget = applyStyleToWidget widget "overflow: hidden;"

-- | Отображение содержимого scroll
overflowScroll :: WidgetUI -> UI WidgetUI
overflowScroll widget = applyStyleToWidget widget "overflow: scroll;"

-- | Отображение содержимого visible
overflowVisible :: WidgetUI -> UI WidgetUI
overflowVisible widget = applyStyleToWidget widget "overflow: visible;"

-- | Отображение содержимого inherit
overflowInherit :: WidgetUI -> UI WidgetUI
overflowInherit widget = applyStyleToWidget widget "overflow: inherit;"



-- | Отображение содержимого X auto
overflowAutoX :: WidgetUI -> UI WidgetUI
overflowAutoX widget = applyStyleToWidget widget "overflow-x: auto;"

-- | Отображение содержимого X hidden
overflowHiddenX :: WidgetUI -> UI WidgetUI
overflowHiddenX widget = applyStyleToWidget widget "overflow-x: hidden;"

-- | Отображение содержимого X scroll
overflowScrollX :: WidgetUI -> UI WidgetUI
overflowScrollX widget = applyStyleToWidget widget "overflow-x: scroll;"

-- | Отображение содержимого X visible
overflowVisibleX :: WidgetUI -> UI WidgetUI
overflowVisibleX widget = applyStyleToWidget widget "overflow-x: visible;"

-- | Отображение содержимого X inherit
overflowInheritX :: WidgetUI -> UI WidgetUI
overflowInheritX widget = applyStyleToWidget widget "overflow-x: inherit;"



-- | Отображение содержимого Y auto
overflowAutoY :: WidgetUI -> UI WidgetUI
overflowAutoY widget = applyStyleToWidget widget "overflow-y: auto;"

-- | Отображение содержимого Y hidden
overflowHiddenY :: WidgetUI -> UI WidgetUI
overflowHiddenY widget = applyStyleToWidget widget "overflow-y: hidden;"

-- | Отображение содержимого Y scroll
overflowScrollY :: WidgetUI -> UI WidgetUI
overflowScrollY widget = applyStyleToWidget widget "overflow-y: scroll;"

-- | Отображение содержимого Y visible
overflowVisibleY :: WidgetUI -> UI WidgetUI
overflowVisibleY widget = applyStyleToWidget widget "overflow-y: visible;"

-- | Отображение содержимого Y inherit
overflowInheritY :: WidgetUI -> UI WidgetUI
overflowInheritY widget = applyStyleToWidget widget "overflow-y: inherit;"



-- | Данные повторения фона
data BGRepeat = BGNoRepeat | BGRepeat | BGRepeatX | BGRepeatY | BGInherit | BGSpace | BGRound

instance Show BGRepeat where
    show BGNoRepeat = "no-repeat"
    show BGRepeat   = "repeat"
    show BGRepeatX  = "repeat-x"
    show BGRepeatY  = "repeat-y"
    show BGInherit  = "inherit"
    show BGSpace    = "space"
    show BGRound    = "round"

-- | Данные прокрутки фона
data BGAttachment = BGAFixed | BGAScroll | BGAInherit | BGALocal

instance Show BGAttachment where
    show BGAFixed   = "fixed"
    show BGAScroll  = "scroll"
    show BGAInherit = "inherit"
    show BGALocal   = "local"

data BGSize = BGSize HSize
            | BGAuto
            | BGCover 
            | BGContain

instance Show BGSize where
    show (BGSize s) = show s
    show BGAuto     = "auto"
    show BGCover    = "cover"
    show BGContain  = "contain"

-- | Дозаполняем атрибуты для background
instance CArgumentableAttr (UIColor -> UIImage -> UIPosition -> BGRepeat -> BGAttachment -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a thC_Or ImageEmpty UIInherit BGNoRepeat BGAFixed
instance CArgumentableAttr (           UIImage -> UIPosition -> BGRepeat -> BGAttachment -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a        ImageEmpty UIInherit BGNoRepeat BGAFixed
instance CArgumentableAttr (                      UIPosition -> BGRepeat -> BGAttachment -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a                   UIInherit BGNoRepeat BGAFixed
instance CArgumentableAttr (                                    BGRepeat -> BGAttachment -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a                             BGNoRepeat BGAFixed
instance CArgumentableAttr (                                                BGAttachment -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a                                        BGAFixed


-- | Фон
background :: UIColor -> UIImage -> UIPosition -> BGRepeat -> BGAttachment -> WidgetUI -> UI WidgetUI
background color image position repeat attachment widget = do
    w1 <- backgroundColor      color      widget
    w2 <- backgroundImage      image      w1
    w3 <- backgroundPosition   position   w2
    w4 <- backgroundRepeat     repeat     w3
    w5 <- backgroundAttachment attachment w4
    return w5

-- | Цвет фона
backgroundColor :: UIColor -> WidgetUI -> UI WidgetUI
backgroundColor color widget = applyStyleToWidget widget ("background-color: " ++ color ++ ";")

-- | Положение фона
backgroundPosition :: UIPosition -> WidgetUI -> UI WidgetUI
backgroundPosition position widget = applyStyleToWidget widget ("background-position: " ++ (show position) ++ ";")

-- | Повторение фона
backgroundRepeat :: BGRepeat -> WidgetUI -> UI WidgetUI
backgroundRepeat repeat widget = applyStyleToWidget widget ("background-repeat: " ++ (show repeat) ++ ";")

-- | Прокрутка фона
backgroundAttachment :: BGAttachment -> WidgetUI -> UI WidgetUI
backgroundAttachment attachment widget =  applyStyleToWidget widget ("background-attachment: " ++ (show attachment) ++ ";")

-- | Изображение фона по URL
backgroundImage :: UIImage -> WidgetUI -> UI WidgetUI
backgroundImage image widget = applyStyleToWidget widget ("background-image: " ++ (show image) ++ ";")

-- | Масштабирует фоновое изображения
backgroundSize :: BGSize -> WidgetUI -> UI WidgetUI
backgroundSize bgSize widget = applyStyleToWidget widget ("background-size: " ++ (show bgSize) ++ ";")


-- | Данные стиля рамки
data BorderStyle = BSNotDefined | BSNone | BSHidden | BSDotted | BSDashed | BSSolid | BSDouble | BSGroove | BSRidge | BSInset | BSOutset | BSInherit
instance Show BorderStyle where
    show BSNotDefined   = ""
    show BSNone         = "none"
    show BSHidden       = "hidden" 
    show BSDotted       = "dotted" 
    show BSDashed       = "dashed" 
    show BSSolid        = "solid" 
    show BSDouble       = "double" 
    show BSGroove       = "groove" 
    show BSRidge        = "ridge" 
    show BSInset        = "inset" 
    show BSOutset       = "outset"
    show BSInherit      = "inherit"



-- | Дозаполняем атрибуты для border
instance CArgumentableAttr (Int -> BorderStyle -> UIColor -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a 1 BSSolid thC_Or 
instance CArgumentableAttr (       BorderStyle -> UIColor -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a   BSSolid thC_Or 
instance CArgumentableAttr (                      UIColor -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a           thC_Or 



-- | Рамка border
border :: Int -> BorderStyle -> UIColor -> WidgetUI -> UI WidgetUI
border  = applyBorderToWidget "border"

-- | Рамка border-left
border_left :: Int -> BorderStyle -> UIColor -> WidgetUI -> UI WidgetUI
border_left  = applyBorderToWidget "border-left"

-- | Рамка border-right
border_right :: Int -> BorderStyle -> UIColor -> WidgetUI -> UI WidgetUI
border_right  = applyBorderToWidget "border-right"

-- | Рамка border-top
border_top :: Int -> BorderStyle -> UIColor -> WidgetUI -> UI WidgetUI
border_top  = applyBorderToWidget "border-top"

-- | Рамка border-bottom
border_bottom :: Int -> BorderStyle -> UIColor -> WidgetUI -> UI WidgetUI
border_bottom  = applyBorderToWidget "border-bottom"

-- | Вспомогательный метод для применения рамки к элементу
applyBorderToWidget :: String -> Int -> BorderStyle -> UIColor -> WidgetUI -> UI WidgetUI
applyBorderToWidget preffix width borderStyle color widget = applyStyleToWidget widget (preffix ++ ": " ++ (show width) ++ "px " ++ (show borderStyle) ++ " " ++ color  ++ ";")

-- | Рамка borderNone
borderNone :: WidgetUI -> UI WidgetUI
borderNone = applyBorderNoneToWidget "border"

-- | Рамка border-leftNone
border_leftNone :: WidgetUI -> UI WidgetUI
border_leftNone = applyBorderNoneToWidget "border-left"

-- | Рамка border-rightNone
border_rightNone :: WidgetUI -> UI WidgetUI
border_rightNone = applyBorderNoneToWidget "border-right"

-- | Рамка border-topNone
border_topNone :: WidgetUI -> UI WidgetUI
border_topNone = applyBorderNoneToWidget "border-top"

-- | Рамка border-bottomNone
border_bottomNone :: WidgetUI -> UI WidgetUI
border_bottomNone = applyBorderNoneToWidget "border-bottom"

-- | Вспомогательный метод для применения рамки None к элементу
applyBorderNoneToWidget :: String -> WidgetUI -> UI WidgetUI
applyBorderNoneToWidget preffix widget = applyStyleToWidget widget (preffix ++ ": " ++ (show 0) ++ "px " ++ (show BSNone) ++ ";")



-- | Радиус рамки
borderRadius :: CSizeable a => (a, a, a, a) -> WidgetUI -> UI WidgetUI
borderRadius (r1, r2, r3, r4) widget =
    applyStyleToWidget widget ("border-radius:" ++ (showSize r1) ++ " " ++ (showSize r2) ++ " " ++ (showSize r3) ++ " " ++ (showSize r4) ++ ";")

-- | Радиус рамки (Num)
borderRadiusNum :: (Int, Int, Int, Int) -> WidgetUI -> UI WidgetUI
borderRadiusNum (r1, r2, r3, r4) widget =
    applyStyleToWidget widget ("border-radius:" ++ (show r1) ++ "px " ++ (show r2) ++ "px " ++ (show r3) ++ "px " ++ (show r4) ++ "px;")

-- | Радиус рамки  (Str)
borderRadiusStr :: (String, String, String, String) -> WidgetUI -> UI WidgetUI
borderRadiusStr (r1, r2, r3, r4) widget =
    applyStyleToWidget widget ("border-radius:" ++ r1 ++ " " ++ r2 ++ " " ++ r3 ++ " " ++ r4 ++ ";")


-- | Радиус рамки (Ext)
borderRadiusExt ::(HSize, HSize, HSize, HSize) -> WidgetUI -> UI WidgetUI
borderRadiusExt (r1, r2, r3, r4) widget =
    applyStyleToWidget widget ("border-radius:" ++ (showSize r1) ++ " " ++ (showSize r2) ++ " " ++ (showSize r3) ++ " " ++ (showSize r4) ++ ";")



-- | Тип точки отчсета для систем координат
data UI_StartingPoint = UI_Default
                      | UI_LT    
                      | UI_RT    
                      | UI_RB    
                      | UI_LB    
                      | UI_Top   
                      | UI_Bottom
                      deriving Show

data HSize = HN Int 
           | HP String
           | HJ Int 
           | HD Double 
           | HL String
           deriving Show

class Show a => CSizeable a where
    showSize :: a -> String

instance CSizeable HSize where
    showSize (HN a) = (show a) ++ "px"
    showSize (HP a) = if '%' `elem` a then a else (a ++ "%")
    showSize (HJ a) = (show a) ++ "%"
    showSize (HD a) = (show a) ++ "%"
    showSize (HL a) = a

instance CSizeable Int where
    showSize a = (show a) ++ "px"
        
instance CSizeable String where
    showSize a = if '%' `elem` a then a else (a ++ "%")

{-
type USize = forall a . Num a => a

t1 :: USize
t1 = 10

t2 :: USize
t2 = "asasas"


instance Num String where
    x + y = x ++ y 
    x - y = x ++ y 
    x * y = x ++ y 
    negate x = x
    abs x = x
    signum x = x
    fromInteger x = show x

instance Data.String.IsString USize where
    fromString _ = 0
-}


-- | Дозаполняем атрибуты для bounds
instance CArgumentableAttr ((Int   , Int   , Int   , Int)    -> UI_StartingPoint -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a (0   , 0   , 100   , 50   ) UI_LT 
instance CArgumentableAttr ((String, String, String, String) -> UI_StartingPoint -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a ("0%", "0%", "50%" , "50%") UI_LT 
instance CArgumentableAttr ((HSize , HSize , HSize , HSize)  -> UI_StartingPoint -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a (HN 0, HN 0, HN 100, HN 50) UI_LT 
--instance (Num b, CSizeable b) => CArgumentableAttr ((b, b, b, b) -> UI_StartingPoint -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a (0, 0, 100, 50) UI_LT 
instance CArgumentableAttr (                                    UI_StartingPoint -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a                             UI_LT 


-- | Разместить виджет (Ext)
bounds :: CSizeable a => (a, a, a, a) -> UI_StartingPoint -> WidgetUI -> UI WidgetUI
bounds (x, y, width, height) startingPoint widget =
    case startingPoint of 
        UI_LT -> applyStyleToWidget widget ("left:"  ++ (showSize x) ++ ";top:"    ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")
        UI_RT -> applyStyleToWidget widget ("right:" ++ (showSize x) ++ ";top:"    ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")
        UI_RB -> applyStyleToWidget widget ("right:" ++ (showSize x) ++ ";bottom:" ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")
        UI_LB -> applyStyleToWidget widget ("left:"  ++ (showSize x) ++ ";bottom:" ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")
        _     -> applyStyleToWidget widget ("left:"  ++ (showSize x) ++ ";top:"    ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")


-- | Разместить виджет (Num)
boundsNum :: (Int, Int, Int, Int) -> UI_StartingPoint -> WidgetUI -> UI WidgetUI
boundsNum (x, y, width, height) startingPoint widget =
    case startingPoint of 
        UI_LT -> applyStyleToWidget widget ("left:"  ++ (show x) ++ "px;top:"    ++ (show y) ++ "px;width:" ++ (show width) ++ "px;height:" ++ (show height) ++ "px;")
        UI_RT -> applyStyleToWidget widget ("right:" ++ (show x) ++ "px;top:"    ++ (show y) ++ "px;width:" ++ (show width) ++ "px;height:" ++ (show height) ++ "px;")
        UI_RB -> applyStyleToWidget widget ("right:" ++ (show x) ++ "px;bottom:" ++ (show y) ++ "px;width:" ++ (show width) ++ "px;height:" ++ (show height) ++ "px;")
        UI_LB -> applyStyleToWidget widget ("left:"  ++ (show x) ++ "px;bottom:" ++ (show y) ++ "px;width:" ++ (show width) ++ "px;height:" ++ (show height) ++ "px;")
        _     -> applyStyleToWidget widget ("left:"  ++ (show x) ++ "px;top:"    ++ (show y) ++ "px;width:" ++ (show width) ++ "px;height:" ++ (show height) ++ "px;")

                  
-- | Разместить виджет (Str)
boundsStr :: (String, String, String, String) -> UI_StartingPoint -> WidgetUI -> UI WidgetUI
boundsStr (x, y, width, height) startingPoint widget =
    case startingPoint of 
        UI_LT -> applyStyleToWidget widget ("left:"  ++ x ++ ";top:"    ++ y ++ ";width:" ++ width ++ ";height:" ++ height ++ ";")
        UI_RT -> applyStyleToWidget widget ("right:" ++ x ++ ";top:"    ++ y ++ ";width:" ++ width ++ ";height:" ++ height ++ ";")
        UI_RB -> applyStyleToWidget widget ("right:" ++ x ++ ";bottom:" ++ y ++ ";width:" ++ width ++ ";height:" ++ height ++ ";")
        UI_LB -> applyStyleToWidget widget ("left:"  ++ x ++ ";bottom:" ++ y ++ ";width:" ++ width ++ ";height:" ++ height ++ ";")
        _     -> applyStyleToWidget widget ("left:"  ++ x ++ ";top:"    ++ y ++ ";width:" ++ width ++ ";height:" ++ height ++ ";")



-- | Разместить виджет (Ext)
boundsExt :: (HSize, HSize, HSize, HSize) -> UI_StartingPoint -> WidgetUI -> UI WidgetUI
boundsExt (x, y, width, height) startingPoint widget =
    case startingPoint of 
        UI_LT -> applyStyleToWidget widget ("left:"  ++ (showSize x) ++ ";top:"    ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")
        UI_RT -> applyStyleToWidget widget ("right:" ++ (showSize x) ++ ";top:"    ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")
        UI_RB -> applyStyleToWidget widget ("right:" ++ (showSize x) ++ ";bottom:" ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")
        UI_LB -> applyStyleToWidget widget ("left:"  ++ (showSize x) ++ ";bottom:" ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")
        _     -> applyStyleToWidget widget ("left:"  ++ (showSize x) ++ ";top:"    ++ (showSize y) ++ ";width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")



-- | Разместить виджет по отступам от граней
boundsBRD :: CSizeable a => (a, a, a, a) -> WidgetUI -> UI WidgetUI
boundsBRD (top, right, bottom, left) widget =
    applyStyleToWidget widget ("top:" ++ (showSize top) ++ ";right:" ++ (showSize right) ++ ";bottom:" ++ (showSize bottom) ++ ";left:" ++ (showSize left) ++ ";")


-- | Разместить виджет по отступам от граней
boundsBRDNum :: (Int, Int, Int, Int) -> WidgetUI -> UI WidgetUI
boundsBRDNum (top, right, bottom, left) widget =
    applyStyleToWidget widget ("top:" ++ (show top) ++ "px;right:" ++ (show right) ++ "px;bottom:" ++ (show bottom) ++ "px;left:" ++ (show left) ++ "px;")


-- | Разместить виджет по отступам от граней (Str)
boundsBRDStr :: (String, String, String, String) -> WidgetUI -> UI WidgetUI
boundsBRDStr (top, right, bottom, left) widget =
    applyStyleToWidget widget ("top:" ++ top ++ ";right:" ++ right ++ ";bottom:" ++ bottom ++ ";left:" ++ left ++ ";")


-- | Разместить виджет по отступам от граней (Ext)
boundsBRDExt ::(HSize, HSize, HSize, HSize) -> WidgetUI -> UI WidgetUI
boundsBRDExt (top, right, bottom, left) widget =
    applyStyleToWidget widget ("top:" ++ (showSize top) ++ ";right:" ++ (showSize right) ++ ";bottom:" ++ (showSize bottom) ++ ";left:" ++ (showSize left) ++ ";")



-- | Ширина
wwidth :: CSizeable a => a -> WidgetUI -> UI WidgetUI
wwidth val widget =
    applyStyleToWidget widget ("width:" ++ (showSize val) ++ ";")

-- | Ширина (Num)
widthNum :: Int -> WidgetUI -> UI WidgetUI
widthNum val widget =
    applyStyleToWidget widget ("width:" ++ (show val) ++ "px;")

-- | Ширина (Str)
widthStr :: String -> WidgetUI -> UI WidgetUI
widthStr val widget =
    applyStyleToWidget widget ("width:" ++ val ++ ";")

-- | Ширина (Ext)
widthExt :: HSize -> WidgetUI -> UI WidgetUI
widthExt val widget =
    applyStyleToWidget widget ("width:" ++ (showSize val) ++ ";")



-- | Ширина Min
wwidthMin :: CSizeable a => a -> WidgetUI -> UI WidgetUI
wwidthMin val widget =
    applyStyleToWidget widget ("min-width:" ++ (showSize val) ++ ";")

-- | Ширина Min (Num)
widthMinNum :: Int -> WidgetUI -> UI WidgetUI
widthMinNum val widget =
    applyStyleToWidget widget ("min-width:" ++ (show val) ++ "px;")

-- | Ширина Min (Str)
widthMinStr :: String -> WidgetUI -> UI WidgetUI
widthMinStr val widget =
    applyStyleToWidget widget ("min-width:" ++ val ++ ";")

-- | Ширина Min (Ext)
widthMinExt :: HSize -> WidgetUI -> UI WidgetUI
widthMinExt val widget =
    applyStyleToWidget widget ("min-width:" ++ (showSize val) ++ ";")



-- | Ширина Max
wwidthMax :: CSizeable a => a -> WidgetUI -> UI WidgetUI
wwidthMax val widget =
    applyStyleToWidget widget ("max-width:" ++ (showSize val) ++ ";")

-- | Ширина Max (Num)
widthMaxNum :: Int -> WidgetUI -> UI WidgetUI
widthMaxNum val widget =
    applyStyleToWidget widget ("max-width:" ++ (show val) ++ "px;")

-- | Ширина Max (Str)
widthMaxStr :: String -> WidgetUI -> UI WidgetUI
widthMaxStr val widget =
    applyStyleToWidget widget ("max-width:" ++ val ++ ";")

-- | Ширина Max (Ext)
widthMaxExt :: HSize -> WidgetUI -> UI WidgetUI
widthMaxExt val widget =
    applyStyleToWidget widget ("max-width:" ++ (showSize val) ++ ";")



-- | Высота
wheight :: CSizeable a => a -> WidgetUI -> UI WidgetUI
wheight val widget =
    applyStyleToWidget widget ("height:" ++ (showSize val) ++ ";")

-- | Высота (Num)
heightNum :: Int -> WidgetUI -> UI WidgetUI
heightNum val widget =
    applyStyleToWidget widget ("height:" ++ (show val) ++ "px;")

-- | Высота (Str)
heightStr :: String -> WidgetUI -> UI WidgetUI
heightStr val widget =
    applyStyleToWidget widget ("height:" ++ val ++ ";")

-- | Высота (Ext)
heightExt :: HSize -> WidgetUI -> UI WidgetUI
heightExt val widget =
    applyStyleToWidget widget ("height:" ++ (showSize val) ++ ";")



-- | Высота Min
wheightMin :: CSizeable a => a -> WidgetUI -> UI WidgetUI
wheightMin val widget =
    applyStyleToWidget widget ("min-height:" ++ (showSize val) ++ ";")

-- | Высота Min (Num)
heightMinNum :: Int -> WidgetUI -> UI WidgetUI
heightMinNum val widget =
    applyStyleToWidget widget ("min-height:" ++ (show val) ++ "px;")

-- | Высота Min (Str)
heightMinStr :: String -> WidgetUI -> UI WidgetUI
heightMinStr val widget =
    applyStyleToWidget widget ("min-height:" ++ val ++ ";")

-- | Высота Min (Ext)
heightMinExt :: HSize -> WidgetUI -> UI WidgetUI
heightMinExt val widget =
    applyStyleToWidget widget ("min-height:" ++ (showSize val) ++ ";")



-- | Высота Max
wheightMax :: CSizeable a => a -> WidgetUI -> UI WidgetUI
wheightMax val widget =
    applyStyleToWidget widget ("max-height:" ++ (showSize val) ++ ";")

-- | Высота Max (Num)
heightMaxNum :: Int -> WidgetUI -> UI WidgetUI
heightMaxNum val widget =
    applyStyleToWidget widget ("max-height:" ++ (show val) ++ "px;")

-- | Высота Max (Str)
heightMaxStr :: String -> WidgetUI -> UI WidgetUI
heightMaxStr val widget =
    applyStyleToWidget widget ("max-height:" ++ val ++ ";")

-- | Высота Max (Ext)
heightMaxExt :: HSize -> WidgetUI -> UI WidgetUI
heightMaxExt val widget =
    applyStyleToWidget widget ("max-height:" ++ (showSize val) ++ ";")



-- | Дозаполняем атрибуты для size
instance CArgumentableAttr ((Int   , Int   ) -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a (50   , 50   ) 
instance CArgumentableAttr ((String, String) -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a ("50%", "50%") 
instance CArgumentableAttr ((HSize , HSize ) -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a (HN 50, HN 50) 


-- | Размер виджета 
wsize :: CSizeable a => (a, a) -> WidgetUI -> UI WidgetUI
wsize (width, height) widget =
    applyStyleToWidget widget ("width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")


-- | Размер виджета (Num)
sizeNum :: (Int, Int) -> WidgetUI -> UI WidgetUI
sizeNum (width, height) widget =
    applyStyleToWidget widget ("width:" ++ (show width) ++ "px;height:" ++ (show height) ++ "px;")

                  
-- | Размер виджета (Str)
sizeStr :: (String, String) -> WidgetUI -> UI WidgetUI
sizeStr (width, height) widget =
    applyStyleToWidget widget ("width:" ++ width ++ ";height:" ++ height ++ ";")


-- | Размер виджета (Ext)
sizeExt :: (HSize, HSize) -> WidgetUI -> UI WidgetUI
sizeExt (width, height) widget =
    applyStyleToWidget widget ("width:" ++ (showSize width) ++ ";height:" ++ (showSize height) ++ ";")



-- | Дозаполняем атрибуты для margin
instance CArgumentableAttr ((Int   , Int   , Int   , Int)    -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a (5   , 5   , 5   , 5   )  
instance CArgumentableAttr ((String, String, String, String) -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a ("0%", "0%", "0%", "0%")  
instance CArgumentableAttr ((HSize , HSize , HSize , HSize)  -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a (HN 5, HN 5, HN 5, HN 5)  


-- | Внешние отступы виджета
margin :: CSizeable a => (a, a, a, a) -> WidgetUI -> UI WidgetUI
margin (l, t, r, b) widget =
    applyStyleToWidget widget ("margin-left:" ++ (showSize l) ++ ";margin-top:" ++ (showSize t) ++ ";margin-right:" ++ (showSize r) ++ ";margin-bottom:" ++ (showSize b) ++ ";")


-- | Внешние отступы виджета (Num)
marginNum :: (Int, Int, Int, Int) -> WidgetUI -> UI WidgetUI
marginNum (l, t, r, b) widget =
    applyStyleToWidget widget ("margin-left:" ++ (show l) ++ "px;margin-top:" ++ (show t) ++ "px;margin-right:" ++ (show r) ++ "px;margin-bottom:" ++ (show b) ++ "px;")
                  

-- | Внещние отступы виджета (Str)
marginStr :: (String, String, String, String) -> WidgetUI -> UI WidgetUI
marginStr (l, t, r, b) widget =
    applyStyleToWidget widget ("margin-left:" ++ l ++ ";margin-top:" ++  t ++ ";margin-right:" ++ r ++ ";margin-bottom:" ++ b ++ ";")


-- | Внешние отступы виджета (Ext)
marginExt :: (HSize, HSize, HSize, HSize) -> WidgetUI -> UI WidgetUI
marginExt (l, t, r, b) widget =
    applyStyleToWidget widget ("margin-left:" ++ (showSize l) ++ ";margin-top:" ++ (showSize t) ++ ";margin-right:" ++ (showSize r) ++ ";margin-bottom:" ++ (showSize b) ++ ";")



-- | Внешние отступы виджета Left
marginLeft :: CSizeable a => a -> WidgetUI -> UI WidgetUI
marginLeft l widget =
    applyStyleToWidget widget ("margin-left:" ++ (showSize l) ++ ";")


-- | Внешние отступы виджета Left (Num)
marginLeftNum :: Int -> WidgetUI -> UI WidgetUI
marginLeftNum l widget =
    applyStyleToWidget widget ("margin-left:" ++ (show l) ++ "px;")
                  

-- | Внещние отступы виджета Left (Str)
marginLeftStr :: String -> WidgetUI -> UI WidgetUI
marginLeftStr l widget =
    applyStyleToWidget widget ("margin-left:" ++ l ++ ";")


-- | Внешние отступы виджета Left (Ext)
marginLeftExt :: HSize -> WidgetUI -> UI WidgetUI
marginLeftExt l widget =
    applyStyleToWidget widget ("margin-left:" ++ (showSize l) ++ ";")



-- | Внешние отступы виджета Right
marginRight :: CSizeable a => a -> WidgetUI -> UI WidgetUI
marginRight r widget =
    applyStyleToWidget widget ("margin-right:" ++ (showSize r) ++ ";")


-- | Внешние отступы виджета Right (Num)
marginRightNum :: Int -> WidgetUI -> UI WidgetUI
marginRightNum r widget =
    applyStyleToWidget widget ("margin-right:" ++ (show r) ++ "px;")
                  

-- | Внещние отступы виджета Right (Str)
marginRightStr :: String -> WidgetUI -> UI WidgetUI
marginRightStr r widget =
    applyStyleToWidget widget ("margin-right:" ++ r ++ ";")


-- | Внешние отступы виджета Right (Ext)
marginRightExt :: HSize -> WidgetUI -> UI WidgetUI
marginRightExt r widget =
    applyStyleToWidget widget ("margin-right:" ++ (showSize r) ++ ";")



-- | Внешние отступы виджета Top
marginTop :: CSizeable a => a -> WidgetUI -> UI WidgetUI
marginTop t widget =
    applyStyleToWidget widget ("margin-top:" ++ (showSize t) ++ ";")


-- | Внешние отступы виджета Top (Num)
marginTopNum :: Int -> WidgetUI -> UI WidgetUI
marginTopNum t widget =
    applyStyleToWidget widget ("margin-top:" ++ (show t) ++ "px;")
                  

-- | Внещние отступы виджета Top (Str)
marginTopStr :: String -> WidgetUI -> UI WidgetUI
marginTopStr t widget =
    applyStyleToWidget widget ("margin-top:" ++  t ++ ";")


-- | Внешние отступы виджета Top (Ext)
marginTopExt :: HSize -> WidgetUI -> UI WidgetUI
marginTopExt t widget =
    applyStyleToWidget widget ("margin-top:" ++ (showSize t) ++ ";")



-- | Внешние отступы виджета Bottom
marginBottom :: CSizeable a => a -> WidgetUI -> UI WidgetUI
marginBottom b widget =
    applyStyleToWidget widget ("margin-bottom:" ++ (showSize b) ++ ";")


-- | Внешние отступы виджета Bottom (Num)
marginBottomNum :: Int -> WidgetUI -> UI WidgetUI
marginBottomNum b widget =
    applyStyleToWidget widget ("margin-bottom:" ++ (show b) ++ "px;")
                  

-- | Внещние отступы виджета Top (Str)
marginBottomStr :: String -> WidgetUI -> UI WidgetUI
marginBottomStr b widget =
    applyStyleToWidget widget ("margin-bottom:" ++ b ++ ";")


-- | Внешние отступы виджета Top (Ext)
marginBottomExt :: HSize -> WidgetUI -> UI WidgetUI
marginBottomExt b widget =
    applyStyleToWidget widget ("margin-bottom:" ++ (showSize b) ++ ";")



-- | Все внешние отступы виджета
marginAll :: CSizeable a => a -> WidgetUI -> UI WidgetUI
marginAll val = margin (val, val, val, val)

-- | Все внешние отступы виджета (Num)
marginAllNum :: Int -> WidgetUI -> UI WidgetUI
marginAllNum val = marginNum (val, val, val, val)

-- | Все внешние отступы виджета (Str)
marginAllStr :: String -> WidgetUI -> UI WidgetUI
marginAllStr val = marginStr (val, val, val, val)

-- | Все внешние отступы виджета (Ext)
marginAllExt :: HSize -> WidgetUI -> UI WidgetUI
marginAllExt val = marginExt (val, val, val, val)



-- | Внутренние отступы виджета
padding :: CSizeable a => (a, a, a, a) -> WidgetUI -> UI WidgetUI
padding (l, t, r, b) widget =
    applyStyleToWidget widget ("padding-left:" ++ (showSize l) ++ ";padding-top:" ++ (showSize t) ++ ";padding-right:" ++ (showSize r) ++ ";padding-bottom:" ++ (showSize b) ++ ";")


-- | Внутренние отступы виджета (Num)
paddingNum :: (Int, Int, Int, Int) -> WidgetUI -> UI WidgetUI
paddingNum (l, t, r, b) widget =
    applyStyleToWidget widget ("padding-left:" ++ (show l) ++ "px;padding-top:" ++ (show t) ++ "px;padding-right:" ++ (show r) ++ "px;padding-bottom:" ++ (show b) ++ "px;")
                  

-- | Внутренние отступы виджета (Str)
paddingStr :: (String, String, String, String) -> WidgetUI -> UI WidgetUI
paddingStr (l, t, r, b) widget =
    applyStyleToWidget widget ("padding-left:" ++ l ++ ";padding-top:" ++  t ++ ";padding-right:" ++ r ++ ";padding-bottom:" ++ b ++ ";")


-- | Внутренние отступы виджета (Ext)
paddingExt :: (HSize, HSize, HSize, HSize) -> WidgetUI -> UI WidgetUI
paddingExt (l, t, r, b) widget =
    applyStyleToWidget widget ("padding-left:" ++ (showSize l) ++ ";padding-top:" ++ (showSize t) ++ ";padding-right:" ++ (showSize r) ++ ";padding-bottom:" ++ (showSize b) ++ ";")



-- | Внутренние отступы виджета Left
paddingLeft :: CSizeable a => a -> WidgetUI -> UI WidgetUI
paddingLeft l widget =
    applyStyleToWidget widget ("padding-left:" ++ (showSize l) ++ ";")


-- | Внутренние отступы виджета Left (Num)
paddingLeftNum :: Int -> WidgetUI -> UI WidgetUI
paddingLeftNum l widget =
    applyStyleToWidget widget ("padding-left:" ++ (show l) ++ "px;")
                  

-- | Внутренние отступы виджета Left (Str)
paddingLeftStr :: String -> WidgetUI -> UI WidgetUI
paddingLeftStr l widget =
    applyStyleToWidget widget ("padding-left:" ++ l ++ ";")


-- | Внутренние отступы виджета Left (Ext)
paddingLeftExt :: HSize -> WidgetUI -> UI WidgetUI
paddingLeftExt l widget =
    applyStyleToWidget widget ("padding-left:" ++ (showSize l) ++ ";")



-- | Внутренние отступы виджета Right
paddingRight :: CSizeable a => a -> WidgetUI -> UI WidgetUI
paddingRight r widget =
    applyStyleToWidget widget ("padding-right:" ++ (showSize r) ++ ";")


-- | Внутренние отступы виджета Right (Num)
paddingRightNum :: Int -> WidgetUI -> UI WidgetUI
paddingRightNum r widget =
    applyStyleToWidget widget ("padding-right:" ++ (show r) ++ "px;")
                  

-- | Внутренние отступы виджета Right (Str)
paddingRightStr :: String -> WidgetUI -> UI WidgetUI
paddingRightStr r widget =
    applyStyleToWidget widget ("padding-right:" ++ r ++ ";")


-- | Внутренние отступы виджета Right (Ext)
paddingRightExt :: HSize -> WidgetUI -> UI WidgetUI
paddingRightExt r widget =
    applyStyleToWidget widget ("padding-right:" ++ (showSize r) ++ ";")



-- | Внутренние отступы виджета Top
paddingTop :: CSizeable a => a -> WidgetUI -> UI WidgetUI
paddingTop t widget =
    applyStyleToWidget widget ("padding-top:" ++ (showSize t) ++ ";")


-- | Внутренние отступы виджета Top (Num)
paddingTopNum :: Int -> WidgetUI -> UI WidgetUI
paddingTopNum t widget =
    applyStyleToWidget widget ("padding-top:" ++ (show t) ++ "px;")
                  

-- | Внутренние отступы виджета Top (Str)
paddingTopStr :: String -> WidgetUI -> UI WidgetUI
paddingTopStr t widget =
    applyStyleToWidget widget ("padding-top:" ++  t ++ ";")


-- | Внутренние отступы виджета Top (Ext)
paddingTopExt :: HSize -> WidgetUI -> UI WidgetUI
paddingTopExt t widget =
    applyStyleToWidget widget ("padding-top:" ++ (showSize t) ++ ";")



-- | Внутренние отступы виджета Bottom
paddingBottom :: CSizeable a => a -> WidgetUI -> UI WidgetUI
paddingBottom b widget =
    applyStyleToWidget widget ("padding-bottom:" ++ (showSize b) ++ ";")


-- | Внутренние отступы виджета Bottom (Num)
paddingBottomNum :: Int -> WidgetUI -> UI WidgetUI
paddingBottomNum b widget =
    applyStyleToWidget widget ("padding-bottom:" ++ (show b) ++ "px;")
                  

-- | Внутренние отступы виджета Top (Str)
paddingBottomStr :: String -> WidgetUI -> UI WidgetUI
paddingBottomStr b widget =
    applyStyleToWidget widget ("padding-bottom:" ++ b ++ ";")


-- | Внутренние отступы виджета Top (Ext)
paddingBottomExt :: HSize -> WidgetUI -> UI WidgetUI
paddingBottomExt b widget =
    applyStyleToWidget widget ("padding-bottom:" ++ (showSize b) ++ ";")



-- | Все внутренние отступы виджета
paddingAll :: CSizeable a => a -> WidgetUI -> UI WidgetUI
paddingAll val = padding (val, val, val, val)

-- | Все внутренние отступы виджета (Num)
paddingAllNum :: Int -> WidgetUI -> UI WidgetUI
paddingAllNum val = paddingNum (val, val, val, val)

-- | Все внутренние отступы виджета (Str)
paddingAllStr :: String -> WidgetUI -> UI WidgetUI
paddingAllStr val = paddingStr (val, val, val, val)

-- | Все внутренние отступы виджета (Ext)
paddingAllExt :: HSize -> WidgetUI -> UI WidgetUI
paddingAllExt val = paddingExt (val, val, val, val)



-- | Дозаполняем атрибуты left
--instance CArgumentableAttr ((CSizeable a => a) -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a 0 

-- | Отступ слева
left :: CSizeable a => a -> WidgetUI -> UI WidgetUI
left val widget =
    applyStyleToWidget widget ("left:" ++ (showSize val) ++ ";")

-- | Отступ слева (Num)
leftNum :: Int -> WidgetUI -> UI WidgetUI
leftNum val widget =
    applyStyleToWidget widget ("left:" ++ (show val) ++ "px;")

-- | Отступ слева (Str)
leftStr :: String -> WidgetUI -> UI WidgetUI
leftStr val widget =
    applyStyleToWidget widget ("left:" ++ val ++ ";")

-- | Отступ слева (Ext)
leftExt :: HSize -> WidgetUI -> UI WidgetUI
leftExt val widget =
    applyStyleToWidget widget ("left:" ++ (showSize val) ++ ";")



-- | Отступ справа
right :: CSizeable a => a -> WidgetUI -> UI WidgetUI
right val widget =
    applyStyleToWidget widget ("right:" ++ (showSize val) ++ ";")

-- | Отступ справа (Num)
rightNum :: Int -> WidgetUI -> UI WidgetUI
rightNum val widget =
    applyStyleToWidget widget ("right:" ++ (show val) ++ "px;")

-- | Отступ справа (Str)
rightStr :: String -> WidgetUI -> UI WidgetUI
rightStr val widget =
    applyStyleToWidget widget ("right:" ++ val ++ ";")

-- | Отступ справа (Ext)
rightExt :: HSize -> WidgetUI -> UI WidgetUI
rightExt val widget =
    applyStyleToWidget widget ("right:" ++ (showSize val) ++ ";")



-- | Отступ сверху
top :: CSizeable a => a -> WidgetUI -> UI WidgetUI
top val widget =
    applyStyleToWidget widget ("top:" ++ (showSize val) ++ ";")

-- | Отступ сверху (Num)
topNum :: Int -> WidgetUI -> UI WidgetUI
topNum val widget =
    applyStyleToWidget widget ("top:" ++ (show val) ++ "px;")

-- | Отступ сверху (Str)
topStr :: String -> WidgetUI -> UI WidgetUI
topStr val widget =
    applyStyleToWidget widget ("top:" ++ val ++ ";")

-- | Отступ сверху (Ext)
topExt :: HSize -> WidgetUI -> UI WidgetUI
topExt val widget =
    applyStyleToWidget widget ("top:" ++ (showSize val) ++ ";")



-- | Отступ снизу
bottom :: CSizeable a => a -> WidgetUI -> UI WidgetUI
bottom val widget =
    applyStyleToWidget widget ("bottom:" ++ (showSize val) ++ ";")

-- | Отступ снизу (Num)
bottomNum :: Int -> WidgetUI -> UI WidgetUI
bottomNum val widget =
    applyStyleToWidget widget ("bottom:" ++ (show val) ++ "px;")

-- | Отступ снизу (Str)
bottomStr :: String -> WidgetUI -> UI WidgetUI
bottomStr val widget =
    applyStyleToWidget widget ("bottom:" ++ val ++ ";")

-- | Отступ снизу (Ext)
bottomExt :: HSize -> WidgetUI -> UI WidgetUI
bottomExt val widget =
    applyStyleToWidget widget ("bottom:" ++ (showSize val) ++ ";")



-- | Установка foreground (цвет текста)
foreground :: UIColor -> WidgetUI -> UI WidgetUI
foreground color widget =
    applyStyleToWidget widget ("color:" ++ color ++ ";")



-- | Установка fill (цвет залтвки SVG)
fill :: UIColor -> WidgetUI -> UI WidgetUI
fill color widget =
    applyStyleToWidget widget ("fill:" ++ color ++ ";")



-- | Тип семейства шрифта
data FFamily = FFM String

instance Show FFamily where
    show (FFM a) = a


-- | Тип размера шрифта
data FSize = FSizePt Double | FSizeEm Double | FSizeEx Double | HSize deriving Show

instance CSizeable FSize where
    showSize (FSizePt a) = (show a) ++ "pt"
    showSize (FSizeEm a) = (show a) ++ "em"
    showSize (FSizeEx a) = (show a) ++ "ex"
    showSize a = showSize a


-- | Тип стиля шрифта
data FontStyle = FNormal | FItalic | FOblique | FInherit
instance Show FontStyle where
    show FNormal    = "normal"
    show FItalic    = "italic"
    show FOblique   = "oblique"
    show FInherit   = "inherit"


-- | Тип насыщенности шрифта
data FontWeight = FWBold 
                | FWBolder 
                | FWLighter 
                | FWNormal 
                | FW100 
                | FW200 
                | FW300 
                | FW400 
                | FW500 
                | FW600 
                | FW700 
                | FW800 
                | FW900

instance Show FontWeight where
    show FWBold     = "bold"
    show FWBolder   = "bolder"
    show FWLighter  = "lighter"
    show FWNormal   = "normal"
    show FW100      = "100"
    show FW200      = "200"
    show FW300      = "300"
    show FW400      = "400"
    show FW500      = "500"
    show FW600      = "600"
    show FW700      = "700"
    show FW800      = "800"
    show FW900      = "900"


-- | Тип шрифта
data UIFont = UIFont FFamily FSize FontStyle FontWeight 

instance Show UIFont where
    show (UIFont family size style weight) =  
                                             (show                style ) ++ " " ++
                                             (show                weight) ++ " " ++
                                             (showSize            size  ) ++ " " ++ 
                                             (wrappInSingleQuotes . show $ family)

ffSerif     = FFM "serif"
ffSansSerif = FFM "sans-serif"
ffCursive   = FFM "cursive"
ffFantasy   = FFM "fantasy"
ffMonospace = FFM "monospace"


-- | Дозаполняем атрибуты для font
instance CArgumentableAttr (UIFont     -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a (UIFont ffSansSerif (FSizeEm 1) FNormal FWNormal)
instance CArgumentableAttr (FFamily    -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a ffSansSerif
instance CArgumentableAttr (FSize      -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a (FSizeEm 1)
instance CArgumentableAttr (FontStyle  -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a FNormal
instance CArgumentableAttr (FontWeight -> WidgetUI -> UI WidgetUI) where expressArgumentAttr a = a FWNormal


-- | Установить шрифт
font :: UIFont -> WidgetUI -> UI WidgetUI
font f widget = 
    applyStyleToWidget widget ("font:" ++ (show f) ++ ";")


-- | Установить семейства шрифтов
fontFamily :: FFamily -> WidgetUI -> UI WidgetUI
fontFamily family widget =
    applyStyleToWidget widget ("font-family:" ++ (show family) ++ ";")


-- | Установить семейства шрифтов
fontSize :: FSize -> WidgetUI -> UI WidgetUI
fontSize size widget =
    applyStyleToWidget widget ("font-size:" ++ (showSize size) ++ ";")


-- | Установить семейства шрифтов
fontStyle :: FontStyle -> WidgetUI -> UI WidgetUI
fontStyle style widget =
    applyStyleToWidget widget ("font-style:" ++ (show style) ++ ";")


-- | Установить насыщенность шрифтов
fontWeight :: FontWeight -> WidgetUI -> UI WidgetUI
fontWeight weight widget =
    applyStyleToWidget widget ("font-weight:" ++ (show weight) ++ ";")



-- | Установка ZIndex
zIndex :: Int -> WidgetUI -> UI WidgetUI
zIndex val widget =
    applyStyleToWidget widget ("z-index:" ++ (show val) ++ ";")



-- | Применить класс к виджету
classToWidget :: String -> WidgetUI -> UI WidgetUI
classToWidget text widget = applyClassToWidget widget (text ++ " ")



-- | Применить стиля к виджету
styleToWidget :: String -> WidgetUI -> UI WidgetUI
styleToWidget text widget = applyStyleToWidget widget text



-- | Всплывающая подсказка
tooltip :: String -> WidgetUI -> UI WidgetUI
tooltip text widget = do
    case wui_element widget of
        EmptyElementUI  -> return widget
        ElementUI a     -> return widget { wui_element = ElementUI   (a ! HA.title (toAttrVal text)) }
        ContainerUI b   -> return widget { wui_element = ContainerUI (b ! HA.title (toAttrVal text)) }



-- | Действие в сценарии hover
--hover :: CArgumentableAttr a => (UI WidgetUI -> a -> UI WidgetUI) -> WidgetUI -> UI WidgetUI
hover :: WidgetUI -> WidgetUI -> UI WidgetUI
hover sample widget = do
    className <- return $ "hfc_" ++ generateUUID_
    return widget { wui_styles = (wui_styles widget) ++ [H.style $ toHtml ("." ++ className ++ ":hover {" ++ (wui_attr_style sample) ++ "}" )] 
                  , wui_attr_class = (wui_attr_class widget) ++ " " ++ className
                  }

-- | Действие в сценарии hover (в монаде UI)
hoverm :: UI WidgetUI -> WidgetUI -> UI WidgetUI
hoverm samplem widget = do
    sample <- samplem
    className <- return $ "hfc_" ++ generateUUID_
    return widget { wui_styles = (wui_styles widget) ++ [H.style $ toHtml ("." ++ className ++ ":hover {" ++ (wui_attr_style sample) ++ "}" )] 
                  , wui_attr_class = (wui_attr_class widget) ++ " " ++ className
                  }



-- | Обернуть текст в одинарные кавычки
wrappInSingleQuotes :: String -> String
wrappInSingleQuotes text = "'" ++ text ++ "'"



-- | Вспомогательный метод для применения класса к элементу
applyClassToWidget :: WidgetUI -> String -> UI WidgetUI
applyClassToWidget widget text = do return widget { wui_attr_class = (wui_attr_class widget) ++ " " ++ text }



-- | Вспомогательный метод для применения стиля к элементу
applyStyleToWidget :: WidgetUI -> String -> UI WidgetUI
applyStyleToWidget widget text = do return widget { wui_attr_style = (wui_attr_style widget) ++ text }



-- | Подготовка HTML из String
--prepareHTML c = TB.string c


-- | Подготовка CSS
prepareCSS a = toHtml $ renderCssUrl undefined a


-- | Подготовка JavaScript
prepareJS b = toHtml $ renderJavascriptUrl undefined b





-----------------------------------------------------------------------------------------------
-- Виджеты UI   -------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- | Начало цепочки виджетов
shellUI :: H.Html 
        -> UI WidgetUI 
shellUI title = do
    widgetThis <- defaultWidgetUI
    return $ widgetThis { wui_title = title }



-- | Начало цепочки виджетов с расширениями
shellExtUI :: H.Html 
           -> [(WidgetUI -> UI WidgetUI)]
           -> UI WidgetUI 
shellExtUI title exts = do
    shell <- shellUI title
    recExt shell exts
    where recExt :: WidgetUI -> [(WidgetUI -> UI WidgetUI)] -> UI WidgetUI
          recExt shell [] = do return shell
          recExt shell (x:xs) = do 
            sh <- requare shell x 
            recExt sh xs
              


-- | Элемент (Element)
wuiElement :: UI WidgetUI
wuiElement = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "element"
    where
        elem = ContainerUI $ H.div 

wuiElement_ = unsafePerformUI $ wuiElement
{-# DEPRECATED wuiElement_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Виджет элемента из HTML
wuiHTML :: H.Html
        -> UI WidgetUI
wuiHTML v = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "wui_html"
    where
        elem = ElementUI $ H.span $ do v

wuiHTML_ = unsafePerformUI . wuiHTML
{-# DEPRECATED wuiHTML_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Виджет скриптов Src 
wuiScriptSrc :: [String]
            -> UI WidgetUI
wuiScriptSrc array = do
    uuid <- generateUUID
    widget <- return $ createWidgetUI elem uuid "script_src"
    recwsrc widget array
    where
        elem = EmptyElementUI 
        recwsrc :: WidgetUI -> [String] -> UI WidgetUI
        recwsrc w [] = do return w
        recwsrc w (x:xs) = do
            wd <- return $ w { wui_scripts = (wui_scripts w) ++ [H.script ! src (toAttrVal x) $ ""] }
            recwsrc wd xs



-- | Виджет скриптов Src 
wuiStyleHref :: [String]
            -> UI WidgetUI
wuiStyleHref array = do
    uuid <- generateUUID
    widget <- return $ createWidgetUI elem uuid "style_href"
    recwstl widget array
    where
        elem = EmptyElementUI 
        recwstl :: WidgetUI -> [String] -> UI WidgetUI
        recwstl w [] = do return w
        recwstl w (x:xs) = do
            wd <- return $ w { wui_styles = (wui_styles w) ++ [ H.link ! rel "stylesheet" ! type_ "text/css" ! href (toAttrVal x)] }
            recwstl wd xs



-- | Виджет скрипта JS
wuiScriptJS :: JavascriptUrl b 
            -> UI WidgetUI
wuiScriptJS js = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "script"
    where
        elem = ElementUI $ H.script ! type_ "text/javascript" $ do prepareJS js



-- | Виджет скриптов JS
wuiScriptJSs :: [JavascriptUrl b]
            -> UI WidgetUI
wuiScriptJSs jsArray = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "script"
    where
        elem = ElementUI $ H.script ! type_ "text/javascript" $ do mapM_ prepareJS jsArray



-- | Виджет скрипта JS
wuiScriptTextJS :: String 
            -> UI WidgetUI
wuiScriptTextJS tjs = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "script_text"
    where
        elem = ElementUI $ H.script ! type_ "text/javascript" $ toHtml tjs



-- | Виджет скриптов JS
wuiScriptTextJSs :: [String]
            -> UI WidgetUI
wuiScriptTextJSs tjsArray = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "script_text"
    where
        elem = ElementUI $ H.script ! type_ "text/javascript" $ do mapM_ (toHtml) tjsArray



-- | Виджет каскадных таблиц стилей CSS
wuiCSS :: CssUrl a
       -> UI WidgetUI
wuiCSS css = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "css"
    where
        elem = ElementUI $ H.style $ do prepareCSS css



-- | Виджет каскадных таблиц стилей CSS
wuiCSSs :: [CssUrl a]
        -> UI WidgetUI
wuiCSSs cssArray = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "css"
    where
        elem = ElementUI $ H.style $ do mapM_ prepareCSS cssArray





