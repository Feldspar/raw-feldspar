module CoDesign where

import qualified Prelude

import Feldspar
import Feldspar.Run
import Feldspar.Hardware
import qualified Feldspar.Hardware.Compile as HW
import qualified Feldspar.Run.Compile      as SW

-- ...
import Data.TypedStruct
import Language.Embedded.Imperative (FreeExp)
import Feldspar.Representation (FreeDict, TypeRepFun)
import Language.Syntactic (Syntactic, Internal, Domain, (:&:), (:<:))
import Feldspar.Primitive.Representation (Primitive)

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | A simple generic program, as in, it can be ran on both the software and
--   hardware backends of Feldspar.
generic
  :: forall sup exp m.
     ( MonadComp exp m
     , NUM exp
       -- ...
     , FreeExp exp
     , FreeDict exp
       -- ...
     , Syntax exp (exp Int8)
     , Int8 ~ Internal (exp Int8)
       -- ...
     , Domain (exp Int8) ~ (sup :&: TypeRepFun)
     , Primitive :<: sup
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
