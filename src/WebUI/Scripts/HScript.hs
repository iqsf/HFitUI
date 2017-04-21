----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Действия 
----------------------------------------------------------------

module WebUI.Scripts.HScript
    ( module ConMonRWS
    , HSL (..)
     
    , TypeHLang  (..)
    , HLanguage  (..)

    , BuilderHSL (..)

    , HBConfig   (..), defaultHBConfig
    , HBLog      (..)

    , HLFinish   (..)

    , runScript
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Control.Monad.RWS                           as ConMonRWS
import           System.IO.Unsafe                               (unsafePerformIO)



-- | Тип монадного трансформера для HScript
-- Формат для RWST монады: RWST r w s m a
type HSL l a = RWST HBConfig [HBLog] l IO a       


-- | Тип языка
data TypeHLang = THLScript 
               | THLBinary 
               | THLOther String
               deriving (Show, Eq)



-- | Данные конфигурации билдера скрипта
data HBConfig = HBConfig { hbc_titleComment :: String   -- ^ Комментарий в заголовке кода скрипта 
                         , hbc_entryLine    :: String   -- ^ Разделитель строк
                         , hbc_tabSpace     :: String   -- ^ Строка табуляции (если пуста, то код будет без табуляции)
                         , hbc_empty        :: String   -- ^ Пустой символ
                         }

-- | Данные конфигурации билдера скрипта
defaultHBConfig :: HBConfig
defaultHBConfig = HBConfig { hbc_titleComment = "Generate by HScript from the HFitUI library" 
                           , hbc_entryLine    = ""
                           , hbc_tabSpace     = "  "
                           , hbc_empty        = ""
                           }


-- | Данные лога билдера скрипта
data HBLog = HBLog String



-- | Класс скриптового языка
class HLanguage a where
    nameHLang  :: a -> String
    typeHLang  :: a -> TypeHLang
    initHLang  :: a 
    emptyHLang :: a
    beginH     :: a
    endH       :: a



-- | Класс билдера скриптов
class BuilderHSL a where
    buildHSL :: HBConfig -> a -> String
    buildHSL hbConf _ = hbc_empty hbConf

    buildHSL_L :: HBConfig -> a -> String
    buildHSL_L _ _ = error "HScript Error! There was a call method \'buildHSL_L\' from class \'BuilderHSL\' for unintended type."

    buildHSL_R :: HBConfig -> a -> String
    buildHSL_R _ _ = error "HScript Error! There was a call method \'buildHSL_R\' from class \'BuilderHSL\' for unintended type."



-- | Данные завершения скрипта        
data HLFinish = HLFinish

instance BuilderHSL HLFinish where
    buildHSL   _ _ = ""
    buildHSL_L _ _ = ""
    buildHSL_R _ _ = ""
                            



-- | Запустить скрипт на его генерацию
runScript :: (HLanguage l, BuilderHSL l, BuilderHSL a) 
          => HBConfig
          -> HSL l a
          -> String
runScript hbConf srcHSL =  
    unsafePerformIO $ do
        (s, log) <- execRWST srcHSL hbConf initHLang
        return $ buildHSL hbConf s 




