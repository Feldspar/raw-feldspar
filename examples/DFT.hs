-- | Discrete Fourier Transform

module DFT
  ( dft
  , idft
  ) where



import Prelude ()

import Feldspar
import Feldspar.Vector



toColVec :: Vector (Data a) -> Matrix a
toColVec = map (replicate 1)

fromColVec :: Matrix a -> Vector (Data a)
fromColVec = map head

-- | Discrete Fourier Transform
dft :: (RealFloat a, PrimType a, PrimType (Complex a)) =>
    Vector (Data (Complex a)) -> Vector (Data (Complex a))
dft vec = fromColVec $ matMul (mat (length vec)) (toColVec vec)
  where
    mat n = indexedMat n n $ \k l ->
        polar 1 (-(2*π * i2n k * i2n l) / i2n n)

-- | Inverse Discrete Fourier Transform
idft :: (RealFloat a, PrimType a, PrimType (Complex a)) =>
    Vector (Data (Complex a)) -> Vector (Data (Complex a))
idft = divLen . map conjugate . dft . map conjugate
  where
    divLen v = map (/ i2n (length v)) v

