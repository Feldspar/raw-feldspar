{-# LANGUAGE DataKinds           #-}

module CoDesign where

import qualified Prelude

import Feldspar
import Feldspar.Run
import Feldspar.Hardware
import qualified Feldspar.Hardware.Compile as HW
import qualified Feldspar.Run.Compile      as SW

-- ...
import Language.Embedded.Hardware.Expression.Represent.Bit

-- ...
import Data.TypedStruct
import Language.Embedded.Imperative (FreeExp)
import Feldspar.Representation
import Language.Syntactic (Syntactic, Internal, Domain, (:&:), (:<:))
import Feldspar.Primitive.Representation (Primitive)


import GHC.TypeLits

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | A simple generic program, as in, it can be ran on both the software and
--   hardware backends of Feldspar.
generic
  :: forall sup exp m.
     ( MonadComp exp m
     , VAL exp
     , NUM exp
       -- ...
     , FreeExp exp
     , FreeDict exp
       -- ...
     , Syntax exp (exp Int8)
     , Int8 ~ Internal (exp Int8)
       -- ...
     , PrimTypeOf exp Int8
     )
  => m ()
generic =
  do let zero = value 0 :: exp Int8
         one  = value 1 :: exp Int8
         
     a <- initRef zero
     b <- initRef one

     u <- getRef a :: m (exp Int8)
     v <- getRef b

     setRef a (u `plus`  v)
     setRef b (u `minus` v)
     
     return ()

-- | Software interp. of 'generic'.
software :: Run ()
software = generic

-- | Hardware interp. of 'generic'.
hardware :: Hardware ()
hardware = generic

--------------------------------------------------------------------------------

testSoftware = SW.icompile software

testHardware = HW.icompile hardware

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

vector :: Hardware ()
vector =
  do let zero = value (bitFromInteger 0) :: HData (Bits 2)
         one  = value (bitFromInteger 1) :: HData (Bits 2)

     a <- initRef zero
     b <- initRef one

     u <- getRef a :: Hardware (HData (Bits 2))
     v <- getRef b

     setRef a (u `plus`  v)
     setRef b (u `minus` v)

     return ()

--------------------------------------------------------------------------------

testVector = HW.icompile vector

--------------------------------------------------------------------------------
