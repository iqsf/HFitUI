----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Типы для Языка JavaScript 
----------------------------------------------------------------

module WebUI.Scripts.JavaScript.HJSTypes
    ( HLangJS           (..)
    , JsName            (..)
    , HJsTitle          (..)

    , HJsEmpty          (..)

    , HJsBegin          (..)
    , HJsEnd            (..)

    , HJsNull           (..)
    , HJsUndefined      (..)
    
    , HJsThis           (..), thisJS

    , HComment          (..), commentJS     , (//)
    , HCommentMultiLine (..), commentMLineJS, (***)

    , (+-+)

    , hl
    , hJS

    , valInt, valInteger, valRational, valDouble
    , valBool
    , valStr, valText
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (strip)
import qualified Data.Text.Lazy                              as DTL

import           WebUI.Scripts.HScript
import           WebUI.Scripts.JavaScript.HJSBuilder
import           WebUI.Scripts.JavaScript.HJSKeywords



----------------------------------------------------------------
-- Основа синтаксисиа языка   ----------------------------------
----------------------------------------------------------------

-- | Корневой тип для JavaScript
data HLangJS = forall a b. (BuilderHSL a, BuilderHSL b) => a :> b
             | forall c.   (BuilderHSL c              ) => HL c
             | HLangJS



-- | Реализуем класс скриптового языка для корневого типа JavaScript
instance HLanguage HLangJS where
    nameHLang _ = "JavaScript"
    typeHLang _ = THLScript
    initHLang   = (HJsTitle :> HJsEmpty)
    emptyHLang  = HL $ HJsEmpty
    beginH      = HL $ HJsBegin
    endH        = HL $ HJsEnd



-- | Реализуем класс билдера скриптов для HLangJS
instance BuilderHSL HLangJS where
    buildHSL hbConf (a :> b) = 
        buildHSL hbConf a ++
        buildHSL hbConf b 
    buildHSL hbConf (HL c) = buildHSL hbConf c
    buildHSL _ HLangJS = ""

    buildHSL_L hbConf (a :> b) =
        buildHSL_L hbConf a ++
        buildHSL_L hbConf b 
    buildHSL_L hbConf (HL c) = buildHSL_L hbConf c
    buildHSL_L _ HLangJS = ""

    buildHSL_R hbConf (a :> b) =
        buildHSL_R hbConf a ++
        buildHSL_R hbConf b 
    buildHSL_R hbConf (HL c) = buildHSL_R hbConf c
    buildHSL_R _ HLangJS = ""



-- | Реализуем класс билдера скриптов для HLangJS
instance BuilderHSL [HLangJS] where
    buildHSL _      []     = ""
    buildHSL hbConf (x:xs) = 
        let bXS = (buildHSL hbConf xs) in
        (buildHSL hbConf x) ++ (if bXS == "" then "" else ", ") ++ bXS

    buildHSL_L _      []     = ""
    buildHSL_L hbConf (x:xs) = 
        let bXS = (buildHSL_L hbConf xs) in
        (buildHSL_L hbConf x) ++ (if bXS == "" then "" else ", ") ++ bXS

    buildHSL_R _      []     = ""
    buildHSL_R hbConf (x:xs) = 
        let bXS = (buildHSL_R hbConf xs) in
        (buildHSL_R hbConf x) ++ (if bXS == "" then "" else ", ") ++ bXS



-- | Имена в JS
type JsName = String



-- | Заголовок скрипта
data HJsTitle = HJsTitle

-- | Реализуем класс билдера скриптов для HJsTitle
instance BuilderHSL HJsTitle where
    buildHSL hbConf _ = 
        hbc_entryLine hbConf ++ 
        if length hTC > 0 then jsComS ++ hTC ++ jsComF ++ hEL else ""
        where
            hEL = hbc_entryLine hbConf  
            hTC = strip . hbc_titleComment $ hbConf



-- | Пустота
data HJsEmpty = HJsEmpty

instance BuilderHSL HJsEmpty where
    buildHSL _ _ = ""



-- | Условное начало
data HJsBegin = HJsBegin

instance BuilderHSL HJsBegin where
    buildHSL _ _ = ""



-- | Условное окончание
data HJsEnd = HJsEnd

instance BuilderHSL HJsEnd where
    buildHSL _ _ = ""



-- | Комментарий однострочный
data HComment = HComment String

instance BuilderHSL HComment where
    buildHSL hbConf (HComment v) = "//" ++ v ++ "\n"

commentJS :: String 
          -> HSL HLangJS ()
commentJS text = do
    cmn <- return $ HL $ HComment text
    modify (:> cmn)
    return ()

(//) = commentJS



-- | Комментарий многострочный
data HCommentMultiLine = HCommentMultiLine String

instance BuilderHSL HCommentMultiLine where
    buildHSL hbConf (HCommentMultiLine v) = "/*" ++ v ++ "*/" ++ (hbc_entryLine hbConf)

commentMLineJS :: String 
               -> HSL HLangJS ()
commentMLineJS text = do
    cmn <- return $ HL $ HCommentMultiLine text
    modify (:> cmn)
    return ()

(***) = commentMLineJS


(+-+) p1 p2 = p1 ++ "\n" ++ p2


-- | Значение Null
data HJsNull = HJsNull

instance BuilderHSL HJsNull where
    buildHSL _ _ = "null"



-- | Значение Undefined
data HJsUndefined = HJsUndefined

instance BuilderHSL HJsUndefined where
    buildHSL _ _ = "undefined"



-- | Значение This
data HJsThis = HJsThis

instance BuilderHSL HJsThis where
    buildHSL _ _ = "this"


-- | Создать синтаксис This
thisJS :: HLangJS
thisJS = hl HJsThis



-- | Обернуть тип в HLangJS
hl :: (BuilderHSL a) 
   => a 
   -> HLangJS
hl p = HL $ p



-- | Обернуть строку как JavaScript код
hJS :: String
    -> HLangJS
hJS = hl



----------------------------------------------------------------
-- Значения   --------------------------------------------------

-- | Значение Int
valInt :: Int
       -> HLangJS
valInt = hl


-- | Значение Integer
valInteger :: Integer
           -> HLangJS
valInteger = hl


-- | Значение Rational
valRational :: Rational
            -> HLangJS
valRational = hl


-- | Значение Double
valDouble :: Double
          -> HLangJS
valDouble = hl



-- | Значение Bool
valBool :: Bool
        -> HLangJS
valBool = hl



-- | Значение String
valStr :: String
       -> HLangJS
valStr txt = hl $ show txt


-- | Значение Text
valText :: DTL.Text
        -> HLangJS
valText txt = hl $ show txt



