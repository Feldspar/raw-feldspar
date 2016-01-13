{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module NumSimpl where



import Control.Monad
import Data.Dynamic
import Data.Int
import Data.Word

import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Language.Syntactic
import Language.Syntactic.Functional
import Language.Syntactic.TypeRep

import Feldspar.Representation
import Feldspar.Optimize
import Feldspar.Frontend ()



data NumExp
    = VAR Int
    | INT Int
    | ADD NumExp NumExp
    | SUB NumExp NumExp
    | MUL NumExp NumExp
    | NEG NumExp
  deriving (Eq, Show)

evalNumExp :: Num a => (Int -> a) -> NumExp -> a
evalNumExp env (VAR v)   = env v
evalNumExp env (INT i)   = fromIntegral i
evalNumExp env (ADD a b) = evalNumExp env a + evalNumExp env b
evalNumExp env (SUB a b) = evalNumExp env a - evalNumExp env b
evalNumExp env (MUL a b) = evalNumExp env a * evalNumExp env b
evalNumExp env (NEG a)   = negate (evalNumExp env a)

num2AST :: (Num a, SmallType a) => NumExp -> ASTF FeldDomain a
num2AST = simplify . unData .
    evalNumExp (\v -> sugarSymTR $ VarT $ fromIntegral v)

genNumExp :: Gen NumExp
genNumExp = sized go
  where
    go s = frequency
        [ -- Variable
          (1, do v <- choose (0,4)
                 return $ VAR v
          )
          -- Literal
        , (1, fmap INT $ elements [-100 .. 100])
        , (s, binOp ADD)
        , (s, binOp SUB)
        , (s, binOp MUL)
        , (s, unOp NEG)
        ]
      where
        binOp op = liftM2 op (go (s `div` 2)) (go (s `div` 2))
        unOp op  = liftM op (go (s-1))

instance Arbitrary NumExp
  where
    arbitrary = genNumExp

    shrink (ADD a b) = a : b : [ADD a' b | a' <- shrink a] ++ [ADD a b' | b' <- shrink b]
    shrink (SUB a b) = a : b : [SUB a' b | a' <- shrink a] ++ [SUB a b' | b' <- shrink b]
    shrink (MUL a b) = a : b : [MUL a' b | a' <- shrink a] ++ [MUL a b' | b' <- shrink b]
    shrink (NEG a)   = a : [NEG a' | a' <- shrink a]
    shrink _         = []

-- Test that numeric expressions are simplified correctly
prop_numExp :: (a ~ Int32) => (a,a,a,a,a) -> NumExp -> Bool
prop_numExp (a,b,c,d,e) numExp =
    evalNumExp env1 numExp == evalOpen env2 (num2AST numExp)
  where
    env1 v = [a,b,c,d,e] !! v
    env2   = zip ([0..] :: [Name]) $ map toDyn [a,b,c,d,e]

-- Test that inexact numeric expressions are handled correctly
--
-- This property fails if one changes `isExact` to `const True`
prop_numExp_inexact :: (a ~ Float) => (a,a,a,a,a) -> NumExp -> Bool
prop_numExp_inexact (a,b,c,d,e) numExp =
    evalNumExp env1 numExp == evalOpen env2 (num2AST numExp)
  where
    env1 v = [a,b,c,d,e] !! v
    env2   = zip ([0..] :: [Name]) $ map toDyn [a,b,c,d,e]

prop_simplify_idempotent_int :: NumExp -> Property
prop_simplify_idempotent_int exp = counterexample (unlines [show e, show $ simplify e])
                                 $ e == simplify e
  where
    e :: ASTF FeldDomain Int32
    e = num2AST exp

prop_simplify_idempotent_word :: NumExp -> Bool
prop_simplify_idempotent_word exp = e == simplify e
  where
    e :: ASTF FeldDomain Word32
    e = num2AST exp

prop_simplify_idempotent_double :: NumExp -> Bool
prop_simplify_idempotent_double exp = e == simplify e
  where
    e :: ASTF FeldDomain Double
    e = num2AST exp

main = $defaultMainGenerator

