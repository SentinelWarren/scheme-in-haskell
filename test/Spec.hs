-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Test.Spec where

import Main hiding (main)
import Test.Hspec
import Data.Foldable
import Primitives

test exprs expected = it (last exprs) $ do
    env <- primitiveBindings
    for_ (init exprs) $ evalString env
    result <- evalString env (last exprs)
    shouldBe result expected

main :: IO ()
main = hspec $ do
  describe "scheme-test" $ do
    test ["(+ 137 349)"] "486"
    test ["(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))"] "57"
    test ["(define size 2)", "size"] "2"
    test ["(define (square x) (* x x))", "(square 4)"] "16"
    test ["(define (square x) (* x x))", "(square (square 3))"] "81"
    test ["(define (square x) (* x x))",
          "(define (sum-of-squares x y) (+ (square x) (square y)))",
          "(sum-of-squares 3 4)"] "25"
    test ["(if #t 7 3)"] "7"
    test ["(if #f 7 3)"] "3"
    test ["(define (abs x) (if (< x 0) (- x) x))",
          "(+ (abs 5) (abs (- 5)))"] "10"
    test ["(define (abs x) (cond ((> x 0) x) ((= x 0) 0) ((< x 0) (- x))))",
          "(abs (- 1))"] "1"
    test ["(define (abs x) (cond ((< x 0) (- x)) (else x)))",
          "(abs (- 10))"] "10"
    test ["(cond (1 1) (else 2))"] "1"
    test ["(if 1 1 2)"] "1"
    test ["(define (forthpower n) (define m (* n n)) (* m m))",
          "(forthpower 3)"] "81"
    test ["(define (square x) (* x x))",
          "(define x 42)",
          "(square 5)",
          "x"] "42"
    test ["(define (square x) (* x x))",
          "(define (fithpower x) (* (square (square x)) x))",
          "(fithpower 4)"] "1024"
    --  parameter definitions should not leak!
    --  basically, local definitions should only be visible inside of the function
    test ["(define m 42)",
          "(define (forthpower n) (define m (* n n)) (* m m))",
          "(forthpower 3)",
          "m"] "42" -- currently produces "9"




