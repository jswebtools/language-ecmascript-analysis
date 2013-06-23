function foo(x) {
  return function (x) {
    return x;
  }
}

/*

> module EnvTest2 where

> import Util.Environment

> main = do
>   stmts <- testFile "Test/EnvTest2.lhs"
>   return $ TestList
>     [ testLabelDiff stmts (1,14) (2,20)
>     , testLabelEq stmts (2,20) (3,12)
>     ]

*/
