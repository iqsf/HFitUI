----------------------------------------------------------------
-- Модуль приложения
-- Графический интерфейс (UI)
-- Базовые менеджеры рвзмещения 
----------------------------------------------------------------

module WebUI.Layouts.LayoutBase
    ( simpleLayoutUI

    , baseLayoutUI
    , baseLayoutUI_2

    , widgetLayoutUI
    , widgetUnsafeLayoutUI
    ) where

-- Импорт модулей
import           Prelude                         as PRL
import           Language.Haskell.TH.Syntax

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



-- | Подготовка CSS
prepareCSS a = toHtml $ renderCssUrl undefined a


-- | Подготовка JavaScript
prepareJS b = toHtml $ renderJavascriptUrl undefined b



----------------------------------------------------------------------------------------------
-- SimpleLayout   ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Простой менеджер размещения UI
-- Формирует страницу согласно размещению
simpleLayoutUI :: H.Html  -- ^ Содержимое для HEAD раздела страницы
               -> H.Html  -- ^ Содержимое для BODY раздела страницы
               -> H.Html  -- ^ Результирующая страница
simpleLayoutUI headContent 
               bodyContent = H.docTypeHtml $ do
    H.head $ do
        headContent
    H.body $ do
        bodyContent



----------------------------------------------------------------------------------------------
-- BaseLayout   ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Базовый менеджер размещения UI
-- Формирует страницу согласно размещению
baseLayoutUI :: H.Html             -- ^ Заголовок страницы
             -> [CssUrl a]         -- ^ Массив стилей для HEAD раздела страницы
             -> [JavascriptUrl b]  -- ^ Массив скриптов для HEAD раздела страницы
             -> [H.Html]           -- ^ Массив содержимого для BODY раздела страницы
             -> H.Html             -- ^ Результирующая страница
baseLayoutUI titleText 
             headStyleArray 
             jsArray  
             bodyContentArray = H.docTypeHtml $ do
    H.head $ do
        H.title titleText
        H.style $ do 
            mapM_ prepareCSS headStyleArray
        H.script ! src "/jquery.js" $ ""
        H.script ! src "/md5.js" $ ""
        H.script ! type_ "text/javascript" $ do
            mapM_ prepareJS jsArray
    H.body $ do
        mapM_ toHtml bodyContentArray



-- | Базовый менеджер размещения UI V_2
-- Формирует страницу согласно размещению
baseLayoutUI_2 :: H.Html             -- ^ Заголовок страницы
               -> [CssUrl a]         -- ^ Массив стилей для HEAD раздела страницы
               -> [JavascriptUrl b]  -- ^ Массив скриптов для HEAD раздела страницы
               -> [H.Html]           -- ^ Массив содержимого для BODY раздела страницы
               -> H.Html             -- ^ Результирующая страница
baseLayoutUI_2 titleText 
               headStyleArray 
               jsArray  
               bodyContentArray = H.docTypeHtml $ do
    H.head $ do
        H.title titleText
        H.script ! src "/jquery.js" $ ""
        H.script ! src "/md5.js" $ ""
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "/ol.css" 
        H.script ! src "/ol.js" $ ""
        H.script ! type_ "text/javascript" $ do
            mapM_ prepareJS jsArray
        H.style $ do 
            mapM_ prepareCSS headStyleArray
    H.body $ do
        mapM_ toHtml bodyContentArray



----------------------------------------------------------------------------------------------
-- WidgetLayout   ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Менеджер размещения UI
-- Формирует страницу из виджетов
widgetLayoutUI :: UI WidgetUI        -- ^ Виджет UI
               -> UI H.Html          -- ^ Результирующая страница
widgetLayoutUI w = do 
    widget   <- w 
    styles   <- return $ wui_styles   widget 
    scripts  <- return $ wui_scripts  widget 
    contents <- return $ wui_contents widget 
    return $ H.docTypeHtml $ do
        H.head $ do
            H.title $ wui_title widget 
            mapM_ (H.toHtml) (stylesWidgetUI  widget)
            mapM_ (H.toHtml) (scriptsWidgetUI widget)
            mapM_ (! type_ "text/javascript") scripts
        H.body ! HA.id (toAttrVal $ wui_id widget) 
               ! TBI.customAttribute "ui_widget" (toAttrVal . wui_ui_name $ widget)
               ! HA.style (toAttrVal . wui_attr_style $ widget) 
               $ do
            mapM_ toHtml contents
            mapM_ buildWidgetUI (wui_children widget)
    where
        stylesWidgetUI :: WidgetUI -> [H.Html]
        stylesWidgetUI wui = (wui_styles wui) ++ (recStylesChildrenUI (wui_children wui))

        recStylesChildrenUI :: [WidgetUI] -> [H.Html]
        recStylesChildrenUI [] = []
        recStylesChildrenUI (x:xs)  = (stylesWidgetUI x) ++ (recStylesChildrenUI xs)


        scriptsWidgetUI :: WidgetUI -> [H.Html]
        scriptsWidgetUI wui = (wui_scripts wui) ++ (recScriptsChildrenUI (wui_children wui))

        recScriptsChildrenUI :: [WidgetUI] -> [H.Html]
        recScriptsChildrenUI [] = []
        recScriptsChildrenUI (x:xs)  = (scriptsWidgetUI x) ++ (recScriptsChildrenUI xs)


        -- Построение виджетов UI содержимого страницы
        buildWidgetUI :: WidgetUI -> H.Html
        buildWidgetUI wui = case (wui_element . packWidgetUI $ wui) of
            ContainerUI f   -> recursiveChildrenUI f (wui_children wui)
            ElementUI e     -> e
            EmptyElementUI  -> recursiveChildrenUI toHtml (wui_children wui)

        -- Рекурсивный обход для построение виджетов UI содержимого страницы
        recursiveChildrenUI :: (H.Html -> H.Html) -> [WidgetUI] -> H.Html
        recursiveChildrenUI ff []    = ff ""
        recursiveChildrenUI ff array = ff $ do mapM_ buildWidgetUI array



-- | Менеджер размещения UI (Unsafe)
-- Формирует страницу из виджетов
widgetUnsafeLayoutUI :: UI WidgetUI        -- ^ Виджет UI
                     -> H.Html             -- ^ Результирующая страница
{-# DEPRECATED widgetUnsafeLayoutUI "This is unsafe! It is not recommended to use this function!" #-}
widgetUnsafeLayoutUI w = 
    H.docTypeHtml $ do
        H.head $ do
            H.title $ wui_title widget 
            --H.style $ do 
            --    mapM_ H.toHtml styles
            --H.script ! type_ "text/javascript" $ do
            --    mapM_ H.toHtml scripts
            --mapM_  (H.toHtml) styles
            mapM_ (H.toHtml) (stylesWidgetUI  widget)
            mapM_ (H.toHtml) (scriptsWidgetUI widget)
            mapM_ (! type_ "text/javascript") scripts
        H.body ! HA.id (toAttrVal $ wui_id widget) 
               ! TBI.customAttribute "ui_widget" (toAttrVal . wui_ui_name $ widget)
               ! HA.style (toAttrVal . wui_attr_style $ widget) 
               $ do
        --H.body $ do
            mapM_ toHtml contents
            mapM_ buildWidgetUI (wui_children widget)
    where
        widget = unsafePerformUI w 

        styles   = wui_styles   widget 
        scripts  = wui_scripts  widget 
        contents = wui_contents widget 

        
        stylesWidgetUI :: WidgetUI -> [H.Html]
        stylesWidgetUI wui = (wui_styles wui) ++ (recStylesChildrenUI (wui_children wui))

        recStylesChildrenUI :: [WidgetUI] -> [H.Html]
        recStylesChildrenUI [] = []
        recStylesChildrenUI (x:xs)  = (stylesWidgetUI x) ++ (recStylesChildrenUI xs)


        scriptsWidgetUI :: WidgetUI -> [H.Html]
        scriptsWidgetUI wui = (wui_scripts wui) ++ (recScriptsChildrenUI (wui_children wui))

        recScriptsChildrenUI :: [WidgetUI] -> [H.Html]
        recScriptsChildrenUI [] = []
        recScriptsChildrenUI (x:xs)  = (scriptsWidgetUI x) ++ (recScriptsChildrenUI xs)


        -- Построение виджетов UI содержимого страницы
        buildWidgetUI :: WidgetUI -> H.Html
        buildWidgetUI wui = case (wui_element . packWidgetUI $ wui) of
            ContainerUI f   -> recursiveChildrenUI f (wui_children wui)
            ElementUI e     -> e
            EmptyElementUI  -> recursiveChildrenUI toHtml (wui_children wui)

        -- Рекурсивный обход для построение виджетов UI содержимого страницы
        recursiveChildrenUI :: (H.Html -> H.Html) -> [WidgetUI] -> H.Html
        recursiveChildrenUI ff []    = ff ""
        recursiveChildrenUI ff array = ff $ do mapM_ buildWidgetUI array



