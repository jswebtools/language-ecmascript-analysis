function foo(x) {
  return x;
}

/*

> module EnvTest1 where

> import Util.Environment

> main = do
>   stmts <- testFile "Test/EnvTest1.lhs"
>   return $ testLabelEq stmts  (1,14) (2,10)

*/
