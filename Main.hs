{-# LANGUAGE OverloadedStrings #-}

module Main where

import Infer
import Types

type TestCase = (Env, Expr, Either String ([Constraint], Type))

cases :: [TestCase]
cases = 
  [([], Nil, Right ([], List "#0"))
  ,([], "hoge", Left "type not found for hoge")
  ,([("hoge", Str)], Ref "hoge", Right ([], Str))
  ,([], Lambda "x" "x", Right ([], "#0" :-> "#0"))
  ,([], App (Lambda "x" "x") "y", Left "type not found for y")
  ,([("y", Str)], App (Lambda "x" "x") "y", Right ([("#0",Str)], "#0"))
  ]
    
{-
  ( [ ("null", List "a" :-> Bool)
    , ("nil", List "b")]
    , ("car", List "c") :-> Alpha "c"]
    , ("cdr", List "d") :-> (Alpha "d")]
    , ("cons", Pair "e" (List "e") :-> List "e")
    ]
  , Fix "map" $
      Lambda "x" $
        Lambda "m" $
          If ("null" $$ "m")
             Nil $
             ("cons" $$ ("f" $$ "car" $$ "m"))
                     $$ ("map" $$ "f") $$ ("cdr" $$ "m")
  )
-}

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
