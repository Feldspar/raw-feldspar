{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}

{-# options_ghc -fwarn-incomplete-patterns #-}

-- | Primitive software expressions.
module Feldspar.Software.Primitive where

import Feldspar.Representation

import Data.Array
import Data.Bits
import Data.Complex
import Data.Int
import Data.Typeable
import Data.Word

import Data.Constraint (Dict (..))

import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD (IArr (..))

import Language.Syntactic
import Language.Syntactic.TH
import Language.Syntactic.Functional

--------------------------------------------------------------------------------
-- * Types.
--------------------------------------------------------------------------------

-- | Representation of primitive supported types.
data SoftwarePrimTypeRep a
  where
    BoolST          :: SoftwarePrimTypeRep Bool
    Int8ST          :: SoftwarePrimTypeRep Int8
    Int16ST         :: SoftwarePrimTypeRep Int16
    Int32ST         :: SoftwarePrimTypeRep Int32
    Int64ST         :: SoftwarePrimTypeRep Int64
    Word8ST         :: SoftwarePrimTypeRep Word8
    Word16ST        :: SoftwarePrimTypeRep Word16
    Word32ST        :: SoftwarePrimTypeRep Word32
    Word64ST        :: SoftwarePrimTypeRep Word64
    FloatST         :: SoftwarePrimTypeRep Float
    DoubleST        :: SoftwarePrimTypeRep Double
    ComplexFloatST  :: SoftwarePrimTypeRep (Complex Float)
    ComplexDoubleST :: SoftwarePrimTypeRep (Complex Double)

deriving instance Eq       (SoftwarePrimTypeRep a)
deriving instance Show     (SoftwarePrimTypeRep a)
deriving instance Typeable (SoftwarePrimTypeRep a)

--------------------------------------------------------------------------------

viewPrimTypeRep :: SoftwarePrimTypeRep a -> PrimTypeView a
viewPrimTypeRep BoolST          = PrimTypeBool
viewPrimTypeRep Int8ST          = PrimTypeIntWord $ IntType $ Int8Type
viewPrimTypeRep Int16ST         = PrimTypeIntWord $ IntType $ Int16Type
viewPrimTypeRep Int32ST         = PrimTypeIntWord $ IntType $ Int32Type
viewPrimTypeRep Int64ST         = PrimTypeIntWord $ IntType $ Int64Type
viewPrimTypeRep Word8ST         = PrimTypeIntWord $ WordType $ Word8Type
viewPrimTypeRep Word16ST        = PrimTypeIntWord $ WordType $ Word16Type
viewPrimTypeRep Word32ST        = PrimTypeIntWord $ WordType $ Word32Type
viewPrimTypeRep Word64ST        = PrimTypeIntWord $ WordType $ Word64Type
viewPrimTypeRep FloatST         = PrimTypeFloating FloatType
viewPrimTypeRep DoubleST        = PrimTypeFloating DoubleType
viewPrimTypeRep ComplexFloatST  = PrimTypeComplex ComplexFloatType
viewPrimTypeRep ComplexDoubleST = PrimTypeComplex ComplexDoubleType

unviewPrimTypeRep :: PrimTypeView a -> SoftwarePrimTypeRep a
unviewPrimTypeRep PrimTypeBool                            = BoolST
unviewPrimTypeRep (PrimTypeIntWord (IntType Int8Type))    = Int8ST
unviewPrimTypeRep (PrimTypeIntWord (IntType Int16Type))   = Int16ST
unviewPrimTypeRep (PrimTypeIntWord (IntType Int32Type))   = Int32ST
unviewPrimTypeRep (PrimTypeIntWord (IntType Int64Type))   = Int64ST
unviewPrimTypeRep (PrimTypeIntWord (WordType Word8Type))  = Word8ST
unviewPrimTypeRep (PrimTypeIntWord (WordType Word16Type)) = Word16ST
unviewPrimTypeRep (PrimTypeIntWord (WordType Word32Type)) = Word32ST
unviewPrimTypeRep (PrimTypeIntWord (WordType Word64Type)) = Word64ST
unviewPrimTypeRep (PrimTypeFloating FloatType)            = FloatST
unviewPrimTypeRep (PrimTypeFloating DoubleType)           = DoubleST
unviewPrimTypeRep (PrimTypeComplex ComplexFloatType)      = ComplexFloatST
unviewPrimTypeRep (PrimTypeComplex ComplexDoubleType)     = ComplexDoubleST

primTypeIntWidth :: SoftwarePrimTypeRep a -> Maybe Int
primTypeIntWidth Int8ST   = Just 8
primTypeIntWidth Int16ST  = Just 16
primTypeIntWidth Int32ST  = Just 32
primTypeIntWidth Int64ST  = Just 64
primTypeIntWidth Word8ST  = Just 8
primTypeIntWidth Word16ST = Just 16
primTypeIntWidth Word32ST = Just 32
primTypeIntWidth Word64ST = Just 64
primTypeIntWidth _        = Nothing

--------------------------------------------------------------------------------

type Length = Word32
type Index  = Word32

--------------------------------------------------------------------------------
