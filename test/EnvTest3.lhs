function foo() {
  return function () {
    return x;
  }
  var x;
}

/*

> module EnvTest3 where

> import Util.Environment

> main = do
>   stmts <- testFile "Test/EnvTest3.lhs"
>   return $ TestList
>     [ testLabelEq stmts (3,12) (5,7)
>     ]

*/
