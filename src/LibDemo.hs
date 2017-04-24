--------------------------------------------------------------
-- Module for demonstrating the work of the library
----------------------------------------------------------------

module LibDemo 
    ( testDemo
    ) where

-- Импорт модулей
import qualified Text.Blaze.Html5                            as H

import           Text.Hamlet
import           Text.Julius

import           WebUI.HFitUI
import           WebUI.Scripts.JavaScript.HJavaScript
import           WebUI.Themes.SolarizedUITheme


-- | Demo test
testDemo :: IO ()
testDemo = do
    res <- testOne
    putStrLn $ show res



testOne :: UI H.Html
testOne = 
    widgetLayoutUI $ do 
        -- Базовые виджеты
        shell                   <- shellExtUI "TestGL (Dyn)" []
        (root, idRoot, _)       <- expandWUI $ wuiPanel <#> boundsBRDNum (44, 0, 0, 0)
                                                        <#> overflowHiddenX
                                                        <#> overflowAutoY

        scriptsOutside          <- wuiScriptSrc [] 

        jsScript                <- wuiScriptTextJS $ runScript defaultHBConfig {hbc_entryLine = "\n"
                                                                               } $ do 
                                        var_b <- newVarInt "b" 10 
                                        var_c <- newVarInt "c" 5
                                        var_cm <- newVar "cm" $ mathACos var_c
                                        (//) "Тестовая переменная"
                                        var_testPer <- newVar "testPer" HJsEmpty
                                        var_st <- newVarStr "st" "TestSt"
                                        flag_1 <- newVarBool "flag_1" True
                                        var_res <- newVar "res" HJsNull

                                        var_res <- eql var_res $ (var_b + var_c) * var_st / var_b
                                        var_res <- eql var_res var_b 

                                        hjs $(juliusFile "templates/test/TestScript.julius") 

                                        hjs $[julius|    
                                            function veryTest (){
                                                console.log("VeryTest");
                                            }
                                        |]

                                        (***) $ "Первый многострочный комментарий" +-+ 
                                                "перед функцией myFunc"
                                        vatFn_myFunc <- functJS "myFunc" [] $ do 
                                            return endH
                                        call vatFn_myFunc []
                                        var_res_myFunc <- eqlMT (varJS "res_myFunc") $ call vatFn_myFunc []

                                        var_testFunc <- eqlMT (varJS "testFunc") $ functJS "" [] $ do 
                                            var_bbb <- newVarInt "bbb" 23 
                                            returnJS thisJS
                                        call var_testFunc []
                                        var_res_testFunc <- eqlMT (varJS "res_testFunc") $ call var_testFunc [5, var_b, valStr "qwe", valInt 19, valBool True]
                                        var_res_new_testFunc <- eqlMT (varJS "res_new_testFunc") $ newJS $ call var_testFunc [5, var_b, valStr "qwe", valInt 19, valBool True]

                                        jsFinish 

        shell `addWUIs` [ root
                        , scriptsOutside
                        , jsScript
                        ]
