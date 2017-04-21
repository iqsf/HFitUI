----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Переменные для Языка JavaScript 
----------------------------------------------------------------

module WebUI.Scripts.JavaScript.HJSVars
    ( HJsEql (..)
    , eqlNew, eqlNewMT
    , eql   , eqlMT

    , HJsVar (..), varJS

    , newVar
    , newVarInt
    , newVarDouble
    , newVarBool
    , newVarStr
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (strip)

import           WebUI.Scripts.HScript
import           WebUI.Scripts.JavaScript.HJSUtils              (upjs)
import           WebUI.Scripts.JavaScript.HJSTypes
import           WebUI.Scripts.JavaScript.HJSBuilder



-- | Данные связи переменных
data HJsEql = forall n v. (BuilderHSL n, BuilderHSL v) => HJsEqlNew n v
            | forall n v. (BuilderHSL n, BuilderHSL v) => HJsEql    n v

instance BuilderHSL HJsEql where
    buildHSL hbConf (HJsEqlNew n v) = 
        "var " ++ (buildHSL hbConf $ HJsEql n v)
    buildHSL hbConf (HJsEql n v) = 
        let value = valEql hbConf v in
        (nameEql hbConf n value) ++ value ++ ";" ++ (hbc_entryLine hbConf)
        where
            nameEql hbConf n v = 
                let res = (buildHSL hbConf n) in
                if PRL.length res == 0
                then error ("HScript Error! There is no name for HJsEql value: (" ++ v ++ ")")
                else res
            valEql hbConf v = 
                let res = (buildHSL hbConf v) in
                if PRL.length res == 0
                then ""
                else " = " ++ res

    buildHSL_L hbConf (HJsEqlNew n _) = buildHSL_L hbConf n
    buildHSL_L hbConf (HJsEql    n _) = buildHSL_L hbConf n

    buildHSL_R hbConf (HJsEqlNew _ v) = buildHSL_R hbConf v
    buildHSL_R hbConf (HJsEql    _ v) = buildHSL_R hbConf v



-- | Новая связь создаваемых переменных
eqlNew :: (BuilderHSL n, BuilderHSL v) 
       => n
       -> v 
       -> HSL HLangJS HLangJS
eqlNew name val = do 
    var <- return $ HL $ HJsEqlNew name val
    modify (:> var)
    return $ HL name



-- | Новая связь создаваемых переменных через монадный трансформер
eqlNewMT :: (BuilderHSL n) 
         => n
         -> HSL HLangJS HLangJS
         -> HSL HLangJS HLangJS
eqlNewMT name val = do 
    c <- ask
    s <- get
    var <- return $ HL $ HJsEqlNew name $ upjs val c s
    modify (:> var)
    return $ HL name



-- | Новая связь переменных
eql :: (BuilderHSL n, BuilderHSL v) 
    => n
    -> v 
    -> HSL HLangJS HLangJS
eql name val = do 
    var <- return $ HL $ HJsEql name val
    modify (:> var)
    return $ HL name



-- | Новая связь переменных через монадный трансформер
eqlMT :: (BuilderHSL n) 
      => n
      -> HSL HLangJS HLangJS 
      -> HSL HLangJS HLangJS
eqlMT name val = do 
    c <- ask
    s <- get
    var <- return $ HL $ HJsEql name $ upjs val c s
    modify (:> var)
    return $ HL name



-- | Данные имени связи переменных
data HJsVar = forall k. (BuilderHSL k) => HJsVar k

instance BuilderHSL HJsVar where
    buildHSL   hbConf (HJsVar k) = buildHSL   hbConf k
    buildHSL_L hbConf (HJsVar k) = buildHSL_L hbConf k
    buildHSL_R hbConf (HJsVar k) = error ("HScript Error! Calling function \'buildHSL_R\' for (" ++ (buildHSL hbConf k) ++ ")" )

-- | Имя переменной
varJS :: JsName
      -> HJsVar
varJS name = HJsVar name



-- | Новая переменная
newVar :: (BuilderHSL v) 
       => JsName
       -> v
       -> HSL HLangJS HLangJS 
newVar name val = eqlNew (varJS name) val




-- | Новая переменная Int
newVarInt :: JsName
          -> Int
          -> HSL HLangJS HLangJS
newVarInt name val = eqlNew (varJS name) val



-- | Новая переменная Double
newVarDouble :: JsName
             -> Double
             -> HSL HLangJS HLangJS
newVarDouble name val = eqlNew (varJS name) val



-- | Новая переменная Bool
newVarBool :: JsName
           -> Bool
           -> HSL HLangJS HLangJS
newVarBool name val = eqlNew (varJS name) val



-- | Новая переменная String
newVarStr :: JsName
          -> String
          -> HSL HLangJS HLangJS
newVarStr name val = eqlNew (varJS name) (show val)



