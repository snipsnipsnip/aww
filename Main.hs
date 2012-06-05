{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Infer
import Eval
import Types

type TestCase = (Env, Expr, Either String Type)

cases :: [TestCase]
cases = 
  [([], Nil, Right (List "#0"))
  ,([], "hoge", Left "type not found for hoge")
  ,([("hoge", Str)], Ref "hoge", Right Str)
  ,([], Lambda "x" "x", Right ("#0" :-> "#0"))
  ,([], (Lambda "x" "x") :$ "y", Left "type not found for y")
  ,([("y", Str)], (Lambda "x" "x") :$ "y", Right Str)
  ,([("y", Str)], (Lambda "x" ("x" :@ "x")) :$ "y", Right (Pair Str Str))
  ]

st :: Env
st = [ ("null", List "a" :-> Bool)
     , ("nil", List "b")
     , ("car", List "c" :-> "c")
     , ("cdr", List "d" :-> "d")
     , ("cons", "e" :-> List "e" :-> List "e")
     ]

str :: Frame
str = declare $ do

  fun "null" $ \v -> case v of
    NilV -> return $ BoolV True
    _ -> return $ BoolV False
  
  fun "car" $ \v -> case v of
    ConsV a _ -> return a
    _ -> fail "car: not a pair"
  
  fun "cdr" $ \v -> case v of
    ConsV _ d -> return d
    _ -> fail "car: not a pair"
  
  fun "cons" $ \a -> return $ FunV $ \b ->
    return $ ConsV a b

  where
  declare :: State Frame a -> Frame
  declare m = execState m []
  fun :: String -> (Value -> E Value) -> State Frame ()
  fun name f = modify ((Var name, FunV f):)

{-
  ( [ ("null", List "a" :-> Bool)
    , ("nil", List "b")]
    , ("car", List "c") :-> Alpha "c"]
    , ("cdr", List "d") :-> (Alpha "d")]
    , ("cons", Pair "e" ((List "e") :-> List "e"))
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
