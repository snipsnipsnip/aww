{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Infer
import Eval
import Types

type TestCase = (Env, Expr, Either String Type)

cases :: [TestCase]
cases = collect $ do
  typed [] (List "#0") Nil
  err [] "type not found for hoge" "hoge"
  
  typed [("hoge", Str)] Str $ Ref "hoge"
  typed [] ("#0" :-> "#0") $ Lambda "x" "x"
  
  err [] "type not found for hoge" $ Lambda "x" "x" :$ "hoge"
  typed [("hoge", Str)] Str $ (Lambda "x" "x") :$ "hoge"
  typed [] (Pair Str Str) $ (Lambda "x" ("x" :@ "x")) :$ StrE "hoge"
  
  typed st (List Str) $ "cons" :$ StrE "fuga" :$ ("cons" :$ StrE "hoge" :$ Nil)
  
  err st "constraints conflict: Alpha e resolves to [Bool,Str]" $
      "cons" :$ BoolE True :$ ("cons" :$ StrE "hoge" :$ Nil)
  
  typed st (List Str :-> List Str) $ Lambda "x" $ "cons" :$ StrE "hoge" :$ "x"
  
  where
  collect m = reverse $ execState m []
  typed env ty expr = modify (((env, expr, Right ty)):)
  err env msg expr = modify ((env, expr, Left msg):)

st :: Env
str :: Frame
(str, st) = declare $ do

  fun "null" (List "a" :-> Bool) $ \v -> case v of
    NilV -> return $ BoolV True
    _ -> return $ BoolV False
  
  fun "car" (List "c" :-> "c") $ \v -> case v of
    ConsV a _ -> return a
    _ -> fail "car: not a pair"
  
  fun "cdr" (List "d" :-> "d") $ \v -> case v of
    ConsV _ d -> return d
    _ -> fail "car: not a pair"
  
  fun "cons" ("e" :-> List "e" :-> List "e") $
    \a -> return $ FunV $ \b ->
      return $ ConsV a b

  where
  declare m = unzip $ execState m []
  fun name ty f = modify (((name, FunV f), (name, ty)):)

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
          If ("null" :$ "m")
             Nil $
             ("cons" :$ ("f" :$ "car" :$ "m"))
                     :$ ("map" :$ "f") :$ ("cdr" :$ "m")
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
