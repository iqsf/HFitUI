----------------------------------------------------------------
-- Модуль приложения
-- Скрипты графического интерфейса (HScript)
-- Математические операции для языка JavaScript 
----------------------------------------------------------------

module WebUI.Scripts.JavaScript.HJSMath
    ( mathAbs
    , mathACos
    , mathACosh  
    , mathASin   
    , mathASinh  
    , mathAtan   
    , mathAtan2  
    , mathAtanh  
    , mathCbrt   
    , mathCeil   
    , mathClz32  
    , mathCos    
    , mathCosH   
    , mathExp    
    , mathExpm1  
    , mathFloor  
    , mathFround 
    , mathHypot  
    , mathImul   
    , mathLog    
    , mathLog10  
    , mathLog1p  
    , mathLog2   
    , mathMax    
    , mathMin    
    , mathPow    
    , mathRandom 
    , mathRound  
    , mathSign   
    , mathSin    
    , mathSinh   
    , mathSqrt   
    , mathTan    
    , mathTanh   
    , mathTrunc  
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (strip)

import           WebUI.Scripts.HScript
import           WebUI.Scripts.JavaScript.HJSTypes
import           WebUI.Scripts.JavaScript.HJSBuilder

    

hbConf = defaultHBConfig



mathAbs = abs

mathACos   x = HL $ "Math.acos("    ++ (buildHSL_L hbConf x) ++ ")"
mathACosh  x = HL $ "Math.acosh()"  ++ (buildHSL_L hbConf x) ++ ")"
mathASin   x = HL $ "Math.asin()"   ++ (buildHSL_L hbConf x) ++ ")"
mathASinh  x = HL $ "Math.asinh()"  ++ (buildHSL_L hbConf x) ++ ")"
mathAtan   x = HL $ "Math.atan()"   ++ (buildHSL_L hbConf x) ++ ")"
mathAtan2  x = HL $ "Math.atan2()"  ++ (buildHSL_L hbConf x) ++ ")"
mathAtanh  x = HL $ "Math.atanh()"  ++ (buildHSL_L hbConf x) ++ ")"
mathCbrt   x = HL $ "Math.cbrt()"   ++ (buildHSL_L hbConf x) ++ ")"
mathCeil   x = HL $ "Math.ceil()"   ++ (buildHSL_L hbConf x) ++ ")"
mathClz32  x = HL $ "Math.clz32()"  ++ (buildHSL_L hbConf x) ++ ")"
mathCos    x = HL $ "Math.cos()"    ++ (buildHSL_L hbConf x) ++ ")"
mathCosH   x = HL $ "Math.cosh()"   ++ (buildHSL_L hbConf x) ++ ")"
mathExp    x = HL $ "Math.exp()"    ++ (buildHSL_L hbConf x) ++ ")"
mathExpm1  x = HL $ "Math.expm1()"  ++ (buildHSL_L hbConf x) ++ ")"
mathFloor  x = HL $ "Math.floor()"  ++ (buildHSL_L hbConf x) ++ ")"
mathFround x = HL $ "Math.fround()" ++ (buildHSL_L hbConf x) ++ ")"
mathHypot  x = HL $ "Math.hypot()"  ++ (buildHSL_L hbConf x) ++ ")"
mathImul   x = HL $ "Math.imul()"   ++ (buildHSL_L hbConf x) ++ ")"
mathLog    x = HL $ "Math.log()"    ++ (buildHSL_L hbConf x) ++ ")"
mathLog10  x = HL $ "Math.log10()"  ++ (buildHSL_L hbConf x) ++ ")"
mathLog1p  x = HL $ "Math.log1p()"  ++ (buildHSL_L hbConf x) ++ ")"
mathLog2   x = HL $ "Math.log2()"   ++ (buildHSL_L hbConf x) ++ ")"
mathMax    x = HL $ "Math.max()"    ++ (buildHSL_L hbConf x) ++ ")"
mathMin    x = HL $ "Math.min()"    ++ (buildHSL_L hbConf x) ++ ")"
mathPow    x = HL $ "Math.pow()"    ++ (buildHSL_L hbConf x) ++ ")"
mathRandom x = HL $ "Math.random()" ++ (buildHSL_L hbConf x) ++ ")" 
mathRound  x = HL $ "Math.round()"  ++ (buildHSL_L hbConf x) ++ ")"
mathSign   x = HL $ "Math.sign()"   ++ (buildHSL_L hbConf x) ++ ")" 
mathSin    x = HL $ "Math.sin()"    ++ (buildHSL_L hbConf x) ++ ")"
mathSinh   x = HL $ "Math.sinh()"   ++ (buildHSL_L hbConf x) ++ ")" 
mathSqrt   x = HL $ "Math.sqrt()"   ++ (buildHSL_L hbConf x) ++ ")"  
mathTan    x = HL $ "Math.tan()"    ++ (buildHSL_L hbConf x) ++ ")" 
mathTanh   x = HL $ "Math.tanh()"   ++ (buildHSL_L hbConf x) ++ ")"  
mathTrunc  x = HL $ "Math.trunc()"  ++ (buildHSL_L hbConf x) ++ ")"   



