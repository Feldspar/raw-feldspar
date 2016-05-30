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
-- * AXI Controller.
--------------------------------------------------------------------------------

axi_light_signature
  :: Signature (
          Signal Bit       -- ^ Global clock signal.
       -> Signal Bit       -- ^ Global reset signal.
       -> Signal (Bits 4)  -- ^ Write address.
       -> Signal (Bits 3)  -- ^ Write channel protection type.
       -> Signal Bit       -- ^ Write address valid.
       -> Signal Bit       -- ^ Write address ready.
       -> Signal (Bits 32) -- ^ Write data.
       -> Signal (Bits 4)  -- ^ Write strobes.
       -> Signal Bit       -- ^ Write valid.
       -> Signal Bit       -- ^ Write ready.
       -> Signal (Bits 2)  -- ^ Write response.
       -> Signal Bit       -- ^ Write response valid.
       -> Signal Bit       -- ^ Response ready.
       -> Signal (Bits 4)  -- ^ Read address.
       -> Signal (Bits 3)  -- ^ Protection type.
       -> Signal Bit       -- ^ Read address valid.
       -> Signal Bit       -- ^ Read address ready.
       -> Signal (Bits 32) -- ^ Read data.
       -> Signal (Bits 2)  -- ^ Read response.
       -> Signal Bit       -- ^ Read valid.
       -> Signal Bit       -- ^ Read ready.    
       -> ()
     )
axi_light_signature =
  uniqueInput  "S_AXI_ACLK"    $ \s_axi_aclk    ->       
  uniqueInput  "S_AXI_ARESETN" $ \s_axi_aresetn -> 
  uniqueInput  "S_AXI_AWADDR"  $ \s_axi_awaddr  ->
  uniqueInput  "S_AXI_AWPROT"  $ \s_axi_awprot  ->
  uniqueInput  "S_AXI_AWVALID" $ \s_axi_awvalid -> 
  uniqueOutput "S_AXI_AWREADY" $ \s_axi_awready ->
  uniqueInput  "S_AXI_WDATA"   $ \s_axi_wdata   ->
  uniqueInput  "S_AXI_WSTRB"   $ \s_axi_wstrb   ->
  uniqueInput  "S_AXI_WVALID"  $ \s_axi_wvalid  ->   
  uniqueOutput "S_AXI_WREADY"  $ \s_axi_wready  ->   
  uniqueOutput "S_AXI_BRESP"   $ \s_axi_bresp   ->     
  uniqueOutput "S_AXI_BVALID"  $ \s_axi_bvalid  ->   
  uniqueInput  "S_AXI_BREADY"  $ \s_axi_bready  ->   
  uniqueInput  "S_AXI_ARADDR"  $ \s_axi_araddr  ->
  uniqueInput  "S_AXI_ARPROT"  $ \s_axi_arprot  ->
  uniqueInput  "S_AXI_ARVALID" $ \s_axi_arvalid ->   
  uniqueOutput "S_AXI_ARREADY" $ \s_axi_arready ->
  uniqueOutput "S_AXI_RDATA"   $ \s_axi_rdata   ->     
  uniqueOutput "S_AXI_RRESP"   $ \s_axi_rresp   ->     
  uniqueOutput "S_AXI_RVALID"  $ \s_axi_rvalid  ->
  uniqueInput  "S_AXI_RREADY"  $ \s_axi_rready  ->   
  ret $ axi_light
    s_axi_aclk s_axi_aresetn
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready s_axi_wdata s_axi_wstrb s_axi_wvalid s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready     

axi_light
  :: Signal Bit       -- ^ Global clock signal.
  -> Signal Bit       -- ^ Global reset signal.
  -> Signal (Bits 4)  -- ^ Write address.
  -> Signal (Bits 3)  -- ^ Write channel protection type.
  -> Signal Bit       -- ^ Write address valid.
  -> Signal Bit       -- ^ Write address ready.
  -> Signal (Bits 32) -- ^ Write data.
  -> Signal (Bits 4)  -- ^ Write strobes.
  -> Signal Bit       -- ^ Write valid.
  -> Signal Bit       -- ^ Write ready.
  -> Signal (Bits 2)  -- ^ Write response.
  -> Signal Bit       -- ^ Write response valid.
  -> Signal Bit       -- ^ Response ready.
  -> Signal (Bits 4)  -- ^ Read address.
  -> Signal (Bits 3)  -- ^ Protection type.
  -> Signal Bit       -- ^ Read address valid.
  -> Signal Bit       -- ^ Read address ready.
  -> Signal (Bits 32) -- ^ Read data.
  -> Signal (Bits 2)  -- ^ Read response.
  -> Signal Bit       -- ^ Read valid.
  -> Signal Bit       -- ^ Read ready.    
  -> Hardware ()
axi_light
    s_axi_aclk   s_axi_aresetn
    s_axi_awaddr s_axi_awprot s_axi_awvalid s_axi_awready s_axi_wdata s_axi_wstrb s_axi_wvalid s_axi_wready
    s_axi_bresp  s_axi_bvalid s_axi_bready
    s_axi_araddr s_axi_arprot s_axi_arvalid s_axi_arready s_axi_rdata
    s_axi_rresp  s_axi_rvalid s_axi_rready
  = do undefined


--------------------------------------------------------------------------------
