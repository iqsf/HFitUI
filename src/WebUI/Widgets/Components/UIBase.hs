----------------------------------------------------------------
-- Модуль приложения
-- Графический интерфейс (UI)
-- Базовые компоненты 
----------------------------------------------------------------

module WebUI.Widgets.Components.UIBase
    ( wuiPanel   , wuiPanel_
    , wuiWidget  , wuiWidget_
    , wuiCanvas  , wuiCanvas_
    , wuiTextCase, wuiTextCase_

    , IMSrc(..) , wuiImage    , wuiImage_
    , TFVal(..) , wuiTextField, wuiTextField_
    , AHref(..) , wuiLinkA    , wuiLinkA_
    ,             wuiLinkATxt , wuiLinkATxt_

    , textCase , textCase_
    , textSpan , textSpan_
    , textBlock, textBlock_
    , textI    , textI_
    , textB    , textB_
    , textBR   , textBR_
    , textWBR  , textWBR_
    ) where

-- Импорт модулей
--import           Import 

import           Prelude                         as PRL

import qualified Text.Blaze                      as TB
import qualified Text.Blaze.Html5                as H
import           Text.Blaze.Html5                
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.Blaze.Html5.Attributes     
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Internal             as TBI

import           WebUI.Widgets.UIWidget



----------------------------------------------------------------
-- ВИДЖЕТЫ   ---------------------------------------------------
----------------------------------------------------------------

-- | Виджет панели (Panel)
wuiPanel :: UI WidgetUI
wuiPanel = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "panel"
    where
        elem = ContainerUI $ H.div 

wuiPanel_ = unsafePerformUI $ wuiPanel
{-# DEPRECATED wuiPanel_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Виджет панели с составной структурой (Widget)
wuiWidget :: UI WidgetUI
wuiWidget = do
    panel <- wuiPanel <#> absolute
                      <#> marginNum (3, 3, 3, 3)
                      <#> boundsBRDNum (0, 0, 0, 0) 
    panelAbsolute <- wuiPanel <#> absolute
    panelResult <- panelAbsolute `addWUI` panel
    makeAggregativeSTypeUI panelResult

wuiWidget_ = unsafePerformUI $ wuiWidget
{-# DEPRECATED wuiWidget_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Виджет холста (Canvas)
wuiCanvas :: UI WidgetUI
wuiCanvas = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "canvas"
    where
        elem = ContainerUI $ H.canvas

wuiCanvas_ = unsafePerformUI $ wuiCanvas
{-# DEPRECATED wuiCanvas_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Панель текста (TextCase)
wuiTextCase :: String -> UI WidgetUI
wuiTextCase val = do
    panel <- wuiPanel
    textCase <- textCase val
    panel `addWUI` textCase

wuiTextCase_ = unsafePerformUI . wuiTextCase
{-# DEPRECATED wuiTextCase_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Данные источника (пути) к графическому файлу
data IMSrc = IMSrc String

instance CArgumentableWidget (IMSrc -> UI WidgetUI) where 
    pack a = a (IMSrc "")  

-- | Виджет изображения
wuiImage :: IMSrc 
         -> UI WidgetUI
wuiImage (IMSrc val) = do
    uuid <- generateUUID
    return $ (createWidgetUI elem uuid "textField") {wui_postBuild = postBuild }
    where
        elem :: ElementUI
        elem = ElementUI $ H.img

        postBuild :: ElementUI -> ElementUI
        postBuild (ElementUI e) = ElementUI $ if val == "" 
                                              then e 
                                              else e ! HA.src (toAttrVal val)

wuiImage_ = unsafePerformUI . wuiImage
{-# DEPRECATED wuiImage_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Данные значения текстового поля
data TFVal = TFVal String

instance CArgumentableWidget (TFVal -> UI WidgetUI) where 
    pack a = a (TFVal "")  

-- | Виджет текстового поля
wuiTextField :: TFVal 
             -> UI WidgetUI
wuiTextField (TFVal val) = do
    uuid <- generateUUID
    return $ (createWidgetUI elem uuid "textField") {wui_postBuild = postBuild }
    where
        elem :: ElementUI
        elem = ElementUI $ H.input 

        postBuild :: ElementUI -> ElementUI
        postBuild (ElementUI e) = ElementUI $ if val == "" 
                                              then e 
                                              else e ! HA.value (toAttrVal val)

wuiTextField_ = unsafePerformUI . wuiTextField
{-# DEPRECATED wuiTextField_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Данные href ссылки A
data AHref = AHref
           | AHUrl String

instance CArgumentableWidget (AHref -> UI WidgetUI) where 
    pack a = a AHref  

instance Show AHref where
    show AHref     = "#"
    show (AHUrl a) = a


-- | Виджет ссылки A
wuiLinkA :: AHref 
         -> UI WidgetUI
wuiLinkA h = do
    uuid <- generateUUID
    return $ (createWidgetUI elem uuid "textField") {wui_postBuild = postBuild }
    where
        elem :: ElementUI
        elem = ContainerUI $ H.a

        postBuild :: ElementUI -> ElementUI
        postBuild (ContainerUI e) = let val = show h in 
                                   ContainerUI $ if val == "" 
                                                 then e 
                                                 else e ! HA.src (toAttrVal val)

wuiLinkA_ = unsafePerformUI . wuiLinkA
{-# DEPRECATED wuiLinkA_ "This is unsafe! It is not recommended to use this function!" #-}


-- | Виджет ссылки A с текстом
wuiLinkATxt :: AHref 
            -> String
            -> UI WidgetUI
wuiLinkATxt h txt = do
    uuid <- generateUUID
    return $ (createWidgetUI elem uuid "textField") {wui_postBuild = postBuild }
    where
        elem :: ElementUI
        elem = ElementUI $ H.a $ toHtml txt

        postBuild :: ElementUI -> ElementUI
        postBuild (ElementUI e) = let val = show h in 
                                  ElementUI $ if val == "" 
                                              then e 
                                              else e ! HA.src (toAttrVal val)

wuiLinkATxt_ h t = unsafePerformUI $ wuiLinkATxt h t
{-# DEPRECATED wuiLinkATxt_ "This is unsafe! It is not recommended to use this function!" #-}



----------------------------------------------------------------
-- ТЕКСТ   -----------------------------------------------------
----------------------------------------------------------------

-- | Текст  
textCase :: String 
         -> UI WidgetUI
textCase val = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "textCase"
    where
        elem = ElementUI $ toHtml val 

textCase_ = unsafePerformUI . textCase
{-# DEPRECATED textCase_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Текст элемента 
textSpan :: String 
         -> UI WidgetUI
textSpan val = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "textSpan"
    where
        elem = ElementUI $ H.span $ toHtml val 

textSpan_ = unsafePerformUI . textSpan
{-# DEPRECATED textSpan_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Текст блочный элемента 
textBlock :: String 
         -> UI WidgetUI
textBlock val = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "textBlock"
    where
        elem = ElementUI $ H.div $ toHtml val 

textBlock_ = unsafePerformUI . textBlock
{-# DEPRECATED textBlock_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Текст элемента наклонный
textI :: String 
      -> UI WidgetUI
textI val = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "textI"
    where
        elem = ElementUI $ H.i $ toHtml val

textI_ = unsafePerformUI . textI
{-# DEPRECATED textI_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Текст элемента наклонный
textB :: String 
      -> UI WidgetUI
textB val = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "textB"
    where
        elem = ElementUI $ H.b $ toHtml val

textB_ = unsafePerformUI . textB
{-# DEPRECATED textB_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Разрыв строки текста 
textBR :: UI WidgetUI
textBR = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "textBR"
    where
        elem = ElementUI $ H.br 

textBR_ = unsafePerformUI $ textBR
{-# DEPRECATED textBR_ "This is unsafe! It is not recommended to use this function!" #-}



-- | Указание допустимого места переноса строки текста 
textWBR :: UI WidgetUI
textWBR = do
    uuid <- generateUUID
    return $ createWidgetUI elem uuid "textWBR"
    where
        elem = ElementUI $ H.wbr 

textWBR_ = unsafePerformUI $ textWBR
{-# DEPRECATED textWBR_ "This is unsafe! It is not recommended to use this function!" #-}



