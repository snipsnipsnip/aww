module Main where

import Infer
import Types

type TestCase = (Env, Expr, Either String Type)

cases :: [TestCase]
cases = 
  [([], Nil, Right (List (Alpha "#0")))
  ,([], Ref "hoge", Left "type not found for var hoge")
  ,([("hoge", Str)], Ref "hoge", Right Str)
  ,([], Lambda "x" (Ref "x"), Right ((Alpha "#0") :-> (Alpha "#0")))
  ,([], App (Lambda "x" (Ref "x")) (Ref "y"), Left "type not found for var y")
  ,([("y", Str)], App (Lambda "x" (Ref "x")) (Ref "y"), Right (Alpha "#0"))
  ]
    
test :: IO ()
test = putStr $ joinLines $ zipWith run [1..] cases
  where 
  joinLines :: [ShowS] -> String
  joinLines = ($ "") . foldr line id
    where
    line a b = a . showChar '\n' . b

  run :: Int -> TestCase -> ShowS
  run i (env, expr, expected) = num . msg
    where
    num = showString "test " . shows i . showString ": "
    result = tryInfer env expr
    msg | result == expected = showString "ok"
        | otherwise = showString "fail (expected: " .
                        shows expected .
                        showString ", actual: " .
                        shows result .
                        showChar ')'
