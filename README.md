# HFitUI

##Introduction
Library for generating a graphical interface on the web for the Haskell programming language.

##Short description
The **HFitUI** uses the blaze library as the result of generating a graphical interface on the web. Using the HFitUI, you construct an interface from the panels, buttons, more complex widget components.The HFitUI has its own Javascript generator from Haskell code based on a set of JavaScript simulation functions.
Below is an example code:

```
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
                                        (//) "Test variable"
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

                                        (***) $ "The first multi-line comment" +-+ 
                                                "before the myFunc function"
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
```

This code generates the following HTML:

```
<!DOCTYPE HTML>
<html><head><title>TestGL (Dyn)</title></head><body id="f98b3765-f949-44ed-b653-7234e250f08e" ui_widget="shell" style=""><div id="42826f90-aa08-4008-b3a7-3c27e62b5d2d" ui_widget="panel" style="top:44px;right:0px;bottom:0px;left:0px;overflow-x: hidden;overflow-y: auto;" class=""></div><script type="text/javascript" id="87a1f750-123a-4148-bcb5-a53f40e23a52" ui_widget="script_text" style="" class="">
/*Generate by HScript from the HFitUI library*/
var b = 10;
var c = 5;
var cm = Math.acos(c);
//Test variable
var testPer;
var st = "TestSt";
var flag_1 = true;
var res = null;
res = (((b + c) * st) / b);
res = b;
function test(){
    console.log("Test");
}
function veryTest (){
    console.log("VeryTest");
}
/*The first multi-line comment
before the myFunc function*/
function myFunc (){
}
myFunc();
res_myFunc = myFunc();
;
testFunc = function (){
  var bbb = 23;
  return this;
}
;
testFunc();
res_testFunc = testFunc(5, b, "qwe", 19, true);
;
res_new_testFunc = new testFunc(5, b, "qwe", 19, true);

;
</script></body></html>
```




