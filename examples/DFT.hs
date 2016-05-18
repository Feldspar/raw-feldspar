-- | Discrete Fourier Transform

module DFT
  ( dft
  , idft
  ) where



import Prelude ()

import Feldspar
import Feldspar.Data.Vector



toColVec :: Pull a -> Pull (Pull a)
toColVec = fmap (replicate 1)

fromColVec :: Pull (Pull a) -> Pull a
fromColVec = fmap head

-- | Discrete Fourier Transform
dft :: (RealFloat a, PrimType a, PrimType (Complex a)) =>
    DPull (Complex a) -> DPull (Complex a)
dft vec = fromColVec $ matMul (mat (length vec)) (toColVec vec)
  where
    mat n = pullMatrix n n $ \k l ->
        polar 1 (-(2*π * i2n k * i2n l) / i2n n)

-- | Inverse Discrete Fourier Transform
idft :: (RealFloat a, PrimType a, PrimType (Complex a)) =>
    DPull (Complex a) -> DPull (Complex a)
idft = divLen . fmap conjugate . dft . fmap conjugate
  where
    divLen v = fmap (/ i2n (length v)) v

