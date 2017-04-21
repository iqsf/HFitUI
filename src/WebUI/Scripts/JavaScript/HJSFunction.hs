----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Функции языка JavaScript 
----------------------------------------------------------------

module WebUI.Scripts.JavaScript.HJSFunction
    ( HJsFunc   (..), functJS
    , HJsCall   (..), call
    , HJsNew    (..), newJS
    , HJsReturn (..), returnJS
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (strip)

import           WebUI.Scripts.HScript
import           WebUI.Scripts.JavaScript.HJSTypes
import           WebUI.Scripts.JavaScript.HJSBuilder
import           WebUI.Scripts.JavaScript.HJSUtils              (smartTabStr)



-- | Функция
data HJsFunc = forall f. (BuilderHSL f) => HJsFunc JsName [HLangJS] f


instance BuilderHSL HJsFunc where
    buildHSL hbConf (HJsFunc n args f) = 
        let nf = if (length n) == 0 then "" else n ++ " " in
        "function " ++ nf ++ "(" ++ "){" ++ (hbc_entryLine hbConf) ++
        (smartTabStr (buildHSL hbConf f) (hbc_tabSpace hbConf)) ++
        "}" ++ (hbc_entryLine hbConf)

    buildHSL_L hbConf (HJsFunc n _ _) = n
    buildHSL_R hbConf (HJsFunc _ _ f) = buildHSL hbConf f



-- | Новая функция
functJS :: JsName
        -> [HLangJS] 
        -> HSL HLangJS HLangJS 
        -> HSL HLangJS HLangJS
functJS name args fnHSL = do
    hbConf <- ask 
    s <- liftIO $ do (_, s, _) <- runRWST fnHSL hbConf HLangJS
                     return s
    fnc <- return $ HL $ HJsFunc name args s
    modify (:> fnc)
    return fnc



-- | Функция
data HJsCall = forall f a. (BuilderHSL f) => HJsCall f [HLangJS] 

instance BuilderHSL HJsCall where
    buildHSL hbConf (HJsCall f args) = 
        (buildHSL_L hbConf f) ++ "(" ++ (buildHSL_L hbConf args) ++ ")" ++ ";" ++ (hbc_entryLine hbConf)

    buildHSL_L hbConf (HJsCall f _   ) = buildHSL_L hbConf f
    buildHSL_R hbConf (HJsCall _ args) = buildHSL_R hbConf args



-- | Вызвать функцию
call :: (BuilderHSL f) 
     => f
     -> [HLangJS]
     -> HSL HLangJS HLangJS
call f args = do
    clf <- return $ HL $ HJsCall f args
    modify (:> clf)
    return clf 



-- | Синтаксис New
data HJsNew = forall n. (BuilderHSL n) => HJsNew n


instance BuilderHSL HJsNew where
    buildHSL hbConf (HJsNew n) = 
        "new " ++ (buildHSL hbConf n) ++ (hbc_entryLine hbConf)

    buildHSL_L hbConf (HJsNew  _) = "new "



-- | Новый синтаксис New
newJS :: HSL HLangJS HLangJS 
      -> HSL HLangJS HLangJS
newJS newHSL = do
    hbConf <- ask 
    s <- liftIO $ do (_, s, _) <- runRWST newHSL hbConf HLangJS
                     return s
    rn <- return $ HL $ HJsNew s
    modify (:> rn)
    return rn



-- | Синтаксис Return
data HJsReturn = forall r. (BuilderHSL r) => HJsReturn r


instance BuilderHSL HJsReturn where
    buildHSL hbConf (HJsReturn r) = 
        "return " ++ (buildHSL hbConf r) ++ ";" ++ (hbc_entryLine hbConf)

    buildHSL_L hbConf (HJsReturn  _) = "return "



-- | Новый синтаксис Return
returnJS :: HLangJS 
         -> HSL HLangJS HLangJS
returnJS ret = do
    rr <- return $ HL $ HJsReturn ret
    modify (:> rr)
    return HLangJS



