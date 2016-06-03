{-# LANGUAGE DataKinds #-}

module LDPC where

import Feldspar
import Feldspar.Run
import Feldspar.Hardware
import qualified Feldspar.Hardware.Compile as HW
import qualified Feldspar.Run.Compile      as SW

import Feldspar.Representation
import Feldspar.Primitive.Representation (Primitive)

import Language.Syntactic (Syntactic, Internal, Domain, (:&:), (:<:))
import Language.Embedded.Hardware.Expression.Represent.Bit
import Language.Embedded.Imperative (FreeExp)

import Data.TypedStruct

import Data.Typeable
import GHC.TypeLits

import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Psi lookup table.
--
-- ^ copied from 'FPGA implementation of a Flexible LDPC decoder.
--------------------------------------------------------------------------------

lut_signature :: Signature (
     Signal (Bits 8)
  -> Signal (Bits 7)
  -> ())
lut_signature =
  input  $ \inp ->
  output $ \out ->
  ret $ lut inp out

lut :: Signal (Bits 8) -> Signal (Bits 7) -> Hardware ()
lut inp out = process (inp .: []) $
  do v <- asSigned inp
     switch v
       [ is (-128) $ out <== value 0 
       , is (-127) $ out <== value 0 
       , is (-126) $ out <== value 0 
       , is (-125) $ out <== value 0 
       , is (-124) $ out <== value 0 
       , is (-123) $ out <== value 0 
       , is (-122) $ out <== value 0 
       , is (-121) $ out <== value 0 
       , is (-120) $ out <== value 0 
       , is (-119) $ out <== value 0 
       , is (-118) $ out <== value 0 
       , is (-117) $ out <== value 0 
       , is (-116) $ out <== value 0 
       , is (-115) $ out <== value 0 
       , is (-114) $ out <== value 0 
       , is (-113) $ out <== value 0 
       , is (-112) $ out <== value 0 
       , is (-111) $ out <== value 0 
       , is (-110) $ out <== value 0 
       , is (-109) $ out <== value 0 
       , is (-108) $ out <== value 0 
       , is (-107) $ out <== value 0 
       , is (-106) $ out <== value 0 
       , is (-105) $ out <== value 0 
       , is (-104) $ out <== value 0 
       , is (-103) $ out <== value 0 
       , is (-102) $ out <== value 0 
       , is (-101) $ out <== value 0 
       , is (-100) $ out <== value 0 
       , is (-99) $ out <== value 0  
       , is (-98) $ out <== value 0  
       , is (-97) $ out <== value 1
       , is (-96) $ out <== value 1
       , is (-95) $ out <== value 1
       , is (-94) $ out <== value 1
       , is (-93) $ out <== value 1
       , is (-92) $ out <== value 1
       , is (-91) $ out <== value 1
       , is (-90) $ out <== value 1
       , is (-89) $ out <== value 1
       , is (-88) $ out <== value 1
       , is (-87) $ out <== value 1
       , is (-86) $ out <== value 1
       , is (-85) $ out <== value 1
       , is (-84) $ out <== value 1
       , is (-83) $ out <== value 1
       , is (-82) $ out <== value 1
       , is (-81) $ out <== value 1
       , is (-80) $ out <== value 1
       , is (-79) $ out <== value 1
       , is (-78) $ out <== value 1
       , is (-77) $ out <== value 1
       , is (-76) $ out <== value 1
       , is (-75) $ out <== value 1
       , is (-74) $ out <== value 1
       , is (-73) $ out <== value 1
       , is (-72) $ out <== value 1
       , is (-71) $ out <== value 1
       , is (-70) $ out <== value 1
       , is (-69) $ out <== value 1
       , is (-68) $ out <== value 1  
       , is (-67) $ out <== value 1  
       , is (-66) $ out <== value 1  
       , is (-65) $ out <== value 1  
       , is (-64) $ out <== value 1  
       , is (-63) $ out <== value 1  
       , is (-62) $ out <== value 1  
       , is (-61) $ out <== value 1  
       , is (-60) $ out <== value 1
       , is (-59) $ out <== value 1
       , is (-58) $ out <== value 1
       , is (-57) $ out <== value 1
       , is (-56) $ out <== value 2
       , is (-55) $ out <== value 2
       , is (-54) $ out <== value 2
       , is (-53) $ out <== value 2
       , is (-52) $ out <== value 2
       , is (-51) $ out <== value 2
       , is (-50) $ out <== value 2
       , is (-49) $ out <== value 2
       , is (-48) $ out <== value 2
       , is (-47) $ out <== value 2
       , is (-46) $ out <== value 2
       , is (-45) $ out <== value 2
       , is (-44) $ out <== value 3
       , is (-43) $ out <== value 3
       , is (-42) $ out <== value 3
       , is (-41) $ out <== value 3
       , is (-40) $ out <== value 3
       , is (-39) $ out <== value 3
       , is (-38) $ out <== value 3
       , is (-37) $ out <== value 3
       , is (-36) $ out <== value 4
       , is (-35) $ out <== value 4
       , is (-34) $ out <== value 4
       , is (-33) $ out <== value 4
       , is (-32) $ out <== value 4
       , is (-31) $ out <== value 5
       , is (-30) $ out <== value 5
       , is (-29) $ out <== value 6
       , is (-28) $ out <== value 6
       , is (-27) $ out <== value 7
       , is (-26) $ out <== value 7
       , is (-25) $ out <== value 8
       , is (-24) $ out <== value 8
       , is (-23) $ out <== value 9
       , is (-22) $ out <== value 9
       , is (-21) $ out <== value 10
       , is (-20) $ out <== value 10
       , is (-19) $ out <== value 11
       , is (-18) $ out <== value 11
       , is (-17) $ out <== value 12
       , is (-16) $ out <== value 12
       , is (-15) $ out <== value 13
       , is (-14) $ out <== value 14
       , is (-13) $ out <== value 15
       , is (-12) $ out <== value 16
       , is (-11) $ out <== value 18
       , is (-10) $ out <== value 20
       , is (-9) $ out <== value 22
       , is (-8) $ out <== value 24
       , is (-7) $ out <== value 26
       , is (-6) $ out <== value 28
       , is (-5) $ out <== value 30
       , is (-4) $ out <== value 32
       , is (-3) $ out <== value 40
       , is (-2) $ out <== value 47
       , is (-1) $ out <== value 79
       , is (0) $ out <== value 127
       , is (127) $ out <== value 0
       , is (126) $ out <== value 0
       , is (125) $ out <== value 0
       , is (124) $ out <== value 0
       , is (123) $ out <== value 0
       , is (122) $ out <== value 0
       , is (121) $ out <== value 0
       , is (120) $ out <== value 0
       , is (119) $ out <== value 0
       , is (118) $ out <== value 0
       , is (117) $ out <== value 0
       , is (116) $ out <== value 0
       , is (115) $ out <== value 0
       , is (114) $ out <== value 0
       , is (113) $ out <== value 0
       , is (112) $ out <== value 0
       , is (111) $ out <== value 0
       , is (110) $ out <== value 0
       , is (109) $ out <== value 0
       , is (108) $ out <== value 0
       , is (107) $ out <== value 0
       , is (106) $ out <== value 0
       , is (105) $ out <== value 0
       , is (104) $ out <== value 0
       , is (103) $ out <== value 0
       , is (102) $ out <== value 0
       , is (101) $ out <== value 0
       , is (100) $ out <== value 0
       , is (99) $ out <== value 0
       , is (98) $ out <== value 0
       , is (97) $ out <== value 1
       , is (96) $ out <== value 1
       , is (95) $ out <== value 1
       , is (94) $ out <== value 1
       , is (93) $ out <== value 1
       , is (92) $ out <== value 1
       , is (91) $ out <== value 1
       , is (90) $ out <== value 1
       , is (89) $ out <== value 1
       , is (88) $ out <== value 1
       , is (87) $ out <== value 1
       , is (86) $ out <== value 1
       , is (85) $ out <== value 1
       , is (84) $ out <== value 1
       , is (83) $ out <== value 1
       , is (82) $ out <== value 1
       , is (81) $ out <== value 1
       , is (80) $ out <== value 1
       , is (79) $ out <== value 1
       , is (78) $ out <== value 1
       , is (77) $ out <== value 1
       , is (76) $ out <== value 1
       , is (75) $ out <== value 1
       , is (74) $ out <== value 1
       , is (73) $ out <== value 1
       , is (72) $ out <== value 1
       , is (71) $ out <== value 1
       , is (70) $ out <== value 1
       , is (69) $ out <== value 1
       , is (68) $ out <== value 1
       , is (67) $ out <== value 1
       , is (66) $ out <== value 1
       , is (65) $ out <== value 1
       , is (64) $ out <== value 1
       , is (63) $ out <== value 1
       , is (62) $ out <== value 1
       , is (61) $ out <== value 1
       , is (60) $ out <== value 1
       , is (59) $ out <== value 1
       , is (58) $ out <== value 1
       , is (57) $ out <== value 1
       , is (56) $ out <== value 2
       , is (55) $ out <== value 2
       , is (54) $ out <== value 2
       , is (53) $ out <== value 2
       , is (52) $ out <== value 2
       , is (51) $ out <== value 2
       , is (50) $ out <== value 2
       , is (49) $ out <== value 2
       , is (48) $ out <== value 2
       , is (47) $ out <== value 2
       , is (46) $ out <== value 2
       , is (45) $ out <== value 2
       , is (44) $ out <== value 3
       , is (43) $ out <== value 3
       , is (42) $ out <== value 3
       , is (41) $ out <== value 3
       , is (40) $ out <== value 3
       , is (39) $ out <== value 3
       , is (38) $ out <== value 3
       , is (37) $ out <== value 3
       , is (36) $ out <== value 4
       , is (35) $ out <== value 4
       , is (34) $ out <== value 4
       , is (33) $ out <== value 4
       , is (32) $ out <== value 4
       , is (31) $ out <== value 5
       , is (30) $ out <== value 5
       , is (29) $ out <== value 6
       , is (28) $ out <== value 6
       , is (27) $ out <== value 7
       , is (26) $ out <== value 7
       , is (25) $ out <== value 8
       , is (24) $ out <== value 8
       , is (23) $ out <== value 9
       , is (22) $ out <== value 9
       , is (21) $ out <== value 10
       , is (20) $ out <== value 10
       , is (19) $ out <== value 11
       , is (18) $ out <== value 11
       , is (17) $ out <== value 12
       , is (16) $ out <== value 12
       , is (15) $ out <== value 13
       , is (14) $ out <== value 14
       , is (13) $ out <== value 15
       , is (12) $ out <== value 16
       , is (11) $ out <== value 18
       , is (10) $ out <== value 20
       , is (9) $ out <== value 22
       , is (8) $ out <== value 24
       , is (7) $ out <== value 26
       , is (6) $ out <== value 28
       , is (5) $ out <== value 30
       , is (4) $ out <== value 32
       , is (3) $ out <== value 40
       , is (2) $ out <== value 47
       , is (1) $ out <== value 79
       ]
  
--------------------------------------------------------------------------------

test = HW.icompile prog
  where
    prog :: Hardware ()
    prog = do component lut_signature
              return ()

--------------------------------------------------------------------------------
