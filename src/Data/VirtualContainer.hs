{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

#ifndef MIN_VERSION_GLASGOW_HASKELL
#define MIN_VERSION_GLASGOW_HASKELL(a,b,c,d) 0
#endif
  -- MIN_VERSION_GLASGOW_HASKELL was introduced in GHC 7.10

#if MIN_VERSION_GLASGOW_HASKELL(7,10,0,0)
#else
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Virtual containers

module Data.VirtualContainer where



import Control.Monad
import Data.Proxy

import Language.Syntactic
import Language.Syntactic.Functional.Tuple

import Data.TypeRep
import Data.TypeRep.Representation
import Data.TypeRep.Types.Tuple
import Language.Syntactic.TypeRep.TupleConversion



--------------------------------------------------------------------------------
-- * Virtual containers
--------------------------------------------------------------------------------

-- | A virtual structured container
--
-- It makes sense for the predicate @p@ to exclude tuple types. That way the
-- the type index @a@ uniquely determines the constructor used. This assumption
-- is used by 'zipVirtual' which is only total if it is impossible for 'Actual'
-- to return the same type as the other constructors.
data Virtual p con a
  where
    -- An actual container
    Actual :: p a => con a -> Virtual p con a
    -- A pair of virtual containers
    VTup2  :: Virtual p con a -> Virtual p con b
           -> Virtual p con (a,b)
    VTup3  :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con (a,b,c)
    VTup4  :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d
           -> Virtual p con (a,b,c,d)
    VTup5  :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e
           -> Virtual p con (a,b,c,d,e)
    VTup6  :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con (a,b,c,d,e,f)
    VTup7  :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con g
           -> Virtual p con (a,b,c,d,e,f,g)
    VTup8  :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con g -> Virtual p con h
           -> Virtual p con (a,b,c,d,e,f,g,h)
    VTup9  :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con g -> Virtual p con h -> Virtual p con i
           -> Virtual p con (a,b,c,d,e,f,g,h,i)
    VTup10 :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con g -> Virtual p con h -> Virtual p con i
           -> Virtual p con j
           -> Virtual p con (a,b,c,d,e,f,g,h,i,j)
    VTup11 :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con g -> Virtual p con h -> Virtual p con i
           -> Virtual p con j -> Virtual p con k
           -> Virtual p con (a,b,c,d,e,f,g,h,i,j,k)
    VTup12 :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con g -> Virtual p con h -> Virtual p con i
           -> Virtual p con j -> Virtual p con k -> Virtual p con l
           -> Virtual p con (a,b,c,d,e,f,g,h,i,j,k,l)
    VTup13 :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con g -> Virtual p con h -> Virtual p con i
           -> Virtual p con j -> Virtual p con k -> Virtual p con l
           -> Virtual p con m
           -> Virtual p con (a,b,c,d,e,f,g,h,i,j,k,l,m)
    VTup14 :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con g -> Virtual p con h -> Virtual p con i
           -> Virtual p con j -> Virtual p con k -> Virtual p con l
           -> Virtual p con m -> Virtual p con n
           -> Virtual p con (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
    VTup15 :: Virtual p con a -> Virtual p con b -> Virtual p con c
           -> Virtual p con d -> Virtual p con e -> Virtual p con f
           -> Virtual p con g -> Virtual p con h -> Virtual p con i
           -> Virtual p con j -> Virtual p con k -> Virtual p con l
           -> Virtual p con m -> Virtual p con n -> Virtual p con o
           -> Virtual p con (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

-- | Select from a 'Virtual' structure
vsel1 :: Virtual p c tup -> Virtual p c (Sel1 tup)
vsel1 (VTup2  a _)     = a
vsel1 (VTup3  a _ _)   = a
vsel1 (VTup4  a _ _ _) = a

-- | Select from a 'Virtual' structure
vsel2 :: Virtual p c tup -> Virtual p c (Sel2 tup)
vsel2 (VTup2  _ b)     = b
vsel2 (VTup3  _ b _)   = b
vsel2 (VTup4  _ b _ _) = b

-- | Select from a 'Virtual' structure
vsel3 :: Virtual p c tup -> Virtual p c (Sel3 tup)
vsel3 (VTup3  _ _ c)   = c
vsel3 (VTup4  _ _ c _) = c

-- | Select from a 'Virtual' structure
vsel4 :: Virtual p c tup -> Virtual p c (Sel4 tup)
vsel4 (VTup4  _ _ _ d) = d

-- | Representation of the structure of a 'Virtual' container
type VirtualRep pred = Virtual pred Proxy

-- | Virtualizable types
class VirtualType_ pred a
  where
    virtRep :: VirtualRep pred a
  -- Not exported since we want a closed class

instance pred a => VirtualType_ pred a
  where
    virtRep = Actual Proxy

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b
    ) =>
      VirtualType_ p (a,b)
  where
    virtRep = VTup2
      virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c
    ) =>
      VirtualType_ p (a,b,c)
  where
    virtRep = VTup3
      virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    ) =>
      VirtualType_ p (a,b,c,d)
  where
    virtRep = VTup4 virtRep virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e
    ) =>
      VirtualType_ p (a,b,c,d,e)
  where
    virtRep = VTup5
      virtRep virtRep virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f
    ) =>
      VirtualType_ p (a,b,c,d,e,f)
  where
    virtRep = VTup6
      virtRep virtRep virtRep virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f, VirtualType_ p g
    ) =>
      VirtualType_ p (a,b,c,d,e,f,g)
  where
    virtRep = VTup7
      virtRep virtRep virtRep virtRep virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f, VirtualType_ p g, VirtualType_ p h
    ) =>
      VirtualType_ p (a,b,c,d,e,f,g,h)
  where
    virtRep = VTup8
      virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f, VirtualType_ p g, VirtualType_ p h
    , VirtualType_ p i
    ) =>
      VirtualType_ p (a,b,c,d,e,f,g,h,i)
  where
    virtRep = VTup9
      virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f, VirtualType_ p g, VirtualType_ p h
    , VirtualType_ p i, VirtualType_ p j
    ) =>
      VirtualType_ p (a,b,c,d,e,f,g,h,i,j)
  where
    virtRep = VTup10
      virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep
      virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f, VirtualType_ p g, VirtualType_ p h
    , VirtualType_ p i, VirtualType_ p j, VirtualType_ p k
    ) =>
      VirtualType_ p (a,b,c,d,e,f,g,h,i,j,k)
  where
    virtRep = VTup11
      virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep
      virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f, VirtualType_ p g, VirtualType_ p h
    , VirtualType_ p i, VirtualType_ p j, VirtualType_ p k, VirtualType_ p l
    ) =>
      VirtualType_ p (a,b,c,d,e,f,g,h,i,j,k,l)
  where
    virtRep = VTup12
      virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep
      virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f, VirtualType_ p g, VirtualType_ p h
    , VirtualType_ p i, VirtualType_ p j, VirtualType_ p k, VirtualType_ p l
    , VirtualType_ p m
    ) =>
      VirtualType_ p (a,b,c,d,e,f,g,h,i,j,k,l,m)
  where
    virtRep = VTup13
      virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep
      virtRep virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f, VirtualType_ p g, VirtualType_ p h
    , VirtualType_ p i, VirtualType_ p j, VirtualType_ p k, VirtualType_ p l
    , VirtualType_ p m, VirtualType_ p n
    ) =>
      VirtualType_ p (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
  where
    virtRep = VTup14
      virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep
      virtRep virtRep virtRep virtRep virtRep

instance {-# OVERLAPS #-}
    ( VirtualType_ p a, VirtualType_ p b, VirtualType_ p c, VirtualType_ p d
    , VirtualType_ p e, VirtualType_ p f, VirtualType_ p g, VirtualType_ p h
    , VirtualType_ p i, VirtualType_ p j, VirtualType_ p k, VirtualType_ p l
    , VirtualType_ p m, VirtualType_ p n, VirtualType_ p o
    ) =>
      VirtualType_ p (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
  where
    virtRep = VTup15
      virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep virtRep
      virtRep virtRep virtRep virtRep virtRep virtRep

-- | Virtualizable types
class    VirtualType_ pred a => VirtualType pred a
instance VirtualType_ pred a => VirtualType pred a



-- 'sugar' assumes that the argument is either a tuple type or a type satisfying
-- @pred@.
instance (Tuple :<: sym, TupleType :<: t, PWitness pred t t) =>
    Syntactic (Virtual pred (ASTFull (sym :&: TypeRep t)) a)
  where
    type Domain   (Virtual pred (ASTFull (sym :&: TypeRep t)) a) = (sym :&: TypeRep t)
    type Internal (Virtual pred (ASTFull (sym :&: TypeRep t)) a) = a

    desugar (Actual a)      = unASTFull a
    desugar (VTup2 a b)     = tup2 (desugar a) (desugar b)
    desugar (VTup3 a b c)   = tup3 (desugar a) (desugar b) (desugar c)
    desugar (VTup4 a b c d) = tup4 (desugar a) (desugar b) (desugar c) (desugar d)

    sugar a = case unTypeRep $ getDecor a of
        Sym tup :$ ta :$ tb             | Just Tup2_t <- prj tup -> VTup2 (sugar $ sel1 a) (sugar $ sel2 a)
        Sym tup :$ ta :$ tb :$ tc       | Just Tup3_t <- prj tup -> VTup3 (sugar $ sel1 a) (sugar $ sel2 a) (sugar $ sel3 a)
        Sym tup :$ ta :$ tb :$ tc :$ td | Just Tup4_t <- prj tup -> VTup4 (sugar $ sel1 a) (sugar $ sel2 a) (sugar $ sel3 a) (sugar $ sel4 a)
        t | Right Dict <- pwit (Proxy :: Proxy pred) (TypeRep t) -> Actual $ sugar a



--------------------------------------------------------------------------------
-- * Operations
--------------------------------------------------------------------------------

-- | Map over a 'Virtual' structure
mapVirtual :: forall pred c1 c2 b .
    (forall a . pred a => c1 a -> c2 a) -> Virtual pred c1 b -> Virtual pred c2 b
mapVirtual f = go
  where
    go :: Virtual pred c1 a -> Virtual pred c2 a
    go (Actual a)          = Actual (f a)
    go (VTup2 v1 v2)       = VTup2 (go v1) (go v2)
    go (VTup3 v1 v2 v3)    = VTup3 (go v1) (go v2) (go v3)
    go (VTup4 v1 v2 v3 v4) = VTup4 (go v1) (go v2) (go v3) (go v4)

-- | Monadic map over a 'Virtual' structure
mapVirtualM :: forall m pred c1 c2 b . Monad m =>
    (forall a . pred a => c1 a -> m (c2 a)) -> Virtual pred c1 b -> m (Virtual pred c2 b)
mapVirtualM f = go
  where
    go :: Virtual pred c1 a -> m (Virtual pred c2 a)
    go (Actual a)          = liftM Actual (f a)
    go (VTup2 v1 v2)       = liftM2 VTup2 (go v1) (go v2)
    go (VTup3 v1 v2 v3)    = liftM3 VTup3 (go v1) (go v2) (go v3)
    go (VTup4 v1 v2 v3 v4) = liftM4 VTup4 (go v1) (go v2) (go v3) (go v4)

-- | Map over a 'Virtual' structure
mapVirtualM_ :: forall m pred cont b . Monad m =>
    (forall a . pred a => cont a -> m ()) -> Virtual pred cont b -> m ()
mapVirtualM_ f = go
  where
    go :: Virtual pred cont a -> m ()
    go (Actual a)          = f a
    go (VTup2 v1 v2)       = go v1 >> go v2
    go (VTup3 v1 v2 v3)    = go v1 >> go v2 >> go v3
    go (VTup4 v1 v2 v3 v4) = go v1 >> go v2 >> go v3 >> go v4

-- This doesn't work for some reason, only if `pred` is constrained to a
-- concrete type. (On the other hand, using `listVirtual` is probably less
-- efficient due to the use of `++`.)

-- mapVirtualM_ :: forall m pred cont b . Monad m =>
--     (forall a . pred a => cont a -> m ()) -> Virtual pred cont b -> m ()
-- mapVirtualM_ f = sequence_ . listVirtual f

-- | Fold a 'Virtual' structure to a list
listVirtual :: forall pred cont b c .
    (forall y . pred y => cont y -> c) -> Virtual pred cont b -> [c]
listVirtual f = go
  where
    go :: Virtual pred cont a -> [c]
    go (Actual a)          = [f a]
    go (VTup2 v1 v2)       = go v1 ++ go v2
    go (VTup3 v1 v2 v3)    = go v1 ++ go v2 ++ go v3
    go (VTup4 v1 v2 v3 v4) = go v1 ++ go v2 ++ go v3 ++ go v4

-- | Zip two 'Virtual' structures
--
-- It is assumed that the predicate @pred@ excludes tuples so that the two
-- arguments are guaranteed to have the same structure.
zipVirtual :: forall pred c1 c2 c3 b
    . (forall a . pred a => c1 a -> c2 a -> c3 a)
    -> Virtual pred c1 b
    -> Virtual pred c2 b
    -> Virtual pred c3 b
zipVirtual f = go
  where
    go :: Virtual pred c1 a -> Virtual pred c2 a -> Virtual pred c3 a
    go (Actual a)          (Actual b)          = Actual (f a b)
    go (VTup2 u1 u2)       (VTup2 v1 v2)       = VTup2 (go u1 v1) (go u2 v2)
    go (VTup3 u1 u2 u3)    (VTup3 v1 v2 v3)    = VTup3 (go u1 v1) (go u2 v2) (go u3 v3)
    go (VTup4 u1 u2 u3 u4) (VTup4 v1 v2 v3 v4) = VTup4 (go u1 v1) (go u2 v2) (go u3 v3) (go u4 v4)

-- | Zip two 'Virtual' structures to a list
--
-- It is assumed that the predicate @pred@ excludes tuples so that the two
-- arguments are guaranteed to have the same structure.
zipListVirtual :: forall pred c1 c2 b r
    . (forall a . pred a => c1 a -> c2 a -> r)
    -> Virtual pred c1 b
    -> Virtual pred c2 b
    -> [r]
zipListVirtual f = go
  where
    go :: Virtual pred c1 a -> Virtual pred c2 a -> [r]
    go (Actual a)          (Actual b)          = [f a b]
    go (VTup2 u1 u2)       (VTup2 v1 v2)       = go u1 v1 ++ go u2 v2
    go (VTup3 u1 u2 u3)    (VTup3 v1 v2 v3)    = go u1 v1 ++ go u2 v2 ++ go u3 v3
    go (VTup4 u1 u2 u3 u4) (VTup4 v1 v2 v3 v4) = go u1 v1 ++ go u2 v2 ++ go u3 v3 ++ go u4 v4

-- | Compare two 'Virtual' structures using a function that compares the
-- 'Actual' elements. If the structures don't match, 'False' is returned.
compareVirtual :: forall pred c1 c2 c d
    . (forall a b . (pred a, pred b) => c1 a -> c2 b -> Bool)
    -> Virtual pred c1 c
    -> Virtual pred c2 d
    -> Bool
compareVirtual f = go
  where
    go :: Virtual pred c1 a -> Virtual pred c2 b -> Bool
    go (Actual a)          (Actual b)          = f a b
    go (VTup2 u1 u2)       (VTup2 v1 v2)       = go u1 v1 && go u2 v2
    go (VTup3 u1 u2 u3)    (VTup3 v1 v2 v3)    = go u1 v1 && go u2 v2 && go u3 v3
    go (VTup4 u1 u2 u3 u4) (VTup4 v1 v2 v3 v4) = go u1 v1 && go u2 v2 && go u3 v3 && go u4 v4
    go _ _ = False

-- | Lift a function operating on containers @con@ to a function operating on
-- virtual containers. It is assumed that @pred@ excludes tuples, so that the
-- virtual containers just contain a single 'Actual' node.
liftVirt :: (pred a, pred b) =>
    (con a -> con b) -> Virtual pred con a -> Virtual pred con b
liftVirt f (Actual a) = Actual (f a)

-- | Lift a function operating on containers @con@ to a function operating on
-- virtual containers. It is assumed that @pred@ excludes tuples, so that the
-- virtual containers just contain a single 'Actual' node.
liftVirt2 :: (pred a, pred b, pred c)
    => (con a -> con b -> con c)
    -> Virtual pred con a -> Virtual pred con b -> Virtual pred con c
liftVirt2 f (Actual a) (Actual b) = Actual (f a b)

