{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Virtual containers

module Data.VirtualContainer where



import Control.Monad.Identity
import Data.Proxy
import Language.Haskell.TH

import Language.Syntactic
import Language.Syntactic.Functional.Tuple

import Data.TypeRep
import Data.TypeRep.Representation
import Data.TypeRep.Types.Tuple
import Language.Syntactic.TypeRep.TupleConversion

import Data.VirtualContainer.TH



--------------------------------------------------------------------------------
-- * Virtual containers
--------------------------------------------------------------------------------

-- | A virtual structured container
--
-- It makes sense for the predicate @p@ to exclude tuple types. That way the
-- the type index @a@ uniquely determines the constructor used. This assumption
-- is used by e.g. 'viewActual' and 'zipVirtual' which are only total if it is
-- impossible for 'Actual' to return the same type as the other constructors.

mkVirtualType 15

-- | Get the content of a 'Virtual' leaf
--
-- This function is only total if tuples are excluded by the predicate @pred@.
viewActual :: pred a => Virtual pred con a -> con a
viewActual (Actual c) = c
  -- Note that this function is well-typed even without the contraint, but then
  -- the constraint is needed to guarantee totality (under the assumption that
  -- tuples are excluded by @pred@).

mkVSel 15

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

-- | Create a 'Virtual' container from a value of a virtualizable type
toVirtual :: forall p a . VirtualType p a => a -> Virtual p Identity a
toVirtual = go (virtRep :: VirtualRep p a) . Identity
  where
    go :: VirtualRep p b -> Identity b -> Virtual p Identity b
    go (Actual _) i = Actual i
    go (VTup2 ra rb) (Identity (a,b)) = VTup2 (go ra (Identity a)) (go rb (Identity b))
    go (VTup3 ra rb rc) (Identity (a,b,c)) = VTup3
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
    go (VTup4 ra rb rc rd) (Identity (a,b,c,d)) = VTup4
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d))
    go (VTup5 ra rb rc rd re) (Identity (a,b,c,d,e)) = VTup5
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e))
    go (VTup6 ra rb rc rd re rf) (Identity (a,b,c,d,e,f)) = VTup6
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
    go (VTup7 ra rb rc rd re rf rg) (Identity (a,b,c,d,e,f,g)) = VTup7
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
        (go rg (Identity g))
    go (VTup8 ra rb rc rd re rf rg rh) (Identity (a,b,c,d,e,f,g,h)) = VTup8
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
        (go rg (Identity g)) (go rh (Identity h))
    go (VTup9 ra rb rc rd re rf rg rh ri) (Identity (a,b,c,d,e,f,g,h,i)) = VTup9
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
        (go rg (Identity g)) (go rh (Identity h)) (go ri (Identity i))
    go (VTup10 ra rb rc rd re rf rg rh ri rj) (Identity (a,b,c,d,e,f,g,h,i,j)) = VTup10
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
        (go rg (Identity g)) (go rh (Identity h)) (go ri (Identity i))
        (go rj (Identity j))
    go (VTup11 ra rb rc rd re rf rg rh ri rj rk) (Identity (a,b,c,d,e,f,g,h,i,j,k)) = VTup11
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
        (go rg (Identity g)) (go rh (Identity h)) (go ri (Identity i))
        (go rj (Identity j)) (go rk (Identity k))
    go (VTup12 ra rb rc rd re rf rg rh ri rj rk rl) (Identity (a,b,c,d,e,f,g,h,i,j,k,l)) = VTup12
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
        (go rg (Identity g)) (go rh (Identity h)) (go ri (Identity i))
        (go rj (Identity j)) (go rk (Identity k)) (go rl (Identity l))
    go (VTup13 ra rb rc rd re rf rg rh ri rj rk rl rm) (Identity (a,b,c,d,e,f,g,h,i,j,k,l,m)) = VTup13
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
        (go rg (Identity g)) (go rh (Identity h)) (go ri (Identity i))
        (go rj (Identity j)) (go rk (Identity k)) (go rl (Identity l))
        (go rm (Identity m))
    go (VTup14 ra rb rc rd re rf rg rh ri rj rk rl rm rn) (Identity (a,b,c,d,e,f,g,h,i,j,k,l,m,n)) = VTup14
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
        (go rg (Identity g)) (go rh (Identity h)) (go ri (Identity i))
        (go rj (Identity j)) (go rk (Identity k)) (go rl (Identity l))
        (go rm (Identity m)) (go rn (Identity n))
    go (VTup15 ra rb rc rd re rf rg rh ri rj rk rl rm rn ro) (Identity (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)) = VTup15
        (go ra (Identity a)) (go rb (Identity b)) (go rc (Identity c))
        (go rd (Identity d)) (go re (Identity e)) (go rf (Identity f))
        (go rg (Identity g)) (go rh (Identity h)) (go ri (Identity i))
        (go rj (Identity j)) (go rk (Identity k)) (go rl (Identity l))
        (go rm (Identity m)) (go rn (Identity n)) (go ro (Identity o))



-- 'sugar' assumes that the argument is either a tuple type or a type satisfying
-- @pred@.
instance (Tuple :<: sym, TupleType :<: t, PWitness pred t t) =>
    Syntactic (Virtual pred (ASTFull (sym :&: TypeRep t)) a)
  where
    type Domain   (Virtual pred (ASTFull (sym :&: TypeRep t)) a) = (sym :&: TypeRep t)
    type Internal (Virtual pred (ASTFull (sym :&: TypeRep t)) a) = a

    desugar (Actual a) = unASTFull a
    -- desugar tup = case tup of
    --   VTup2 a b -> tup2 (desugar a) (desugar b)
    --   ...
    desugar tup =
      $( virtualCases 15 (VarE 'tup)
          (\w -> foldl AppE (VarE (mkName ("tup" ++ show w)))
               . map (AppE (VarE 'desugar))
          )
       )

    sugar a = simpleMatch (go t) $ unTypeRep t
      where
        t = getDecor a
        go :: (DenResult sig ~ a)
           => TypeRep t a
           -> t sig
           -> Args (AST t) sig
           -> Virtual pred (ASTFull (sym :&: TypeRep t)) a
        go _ tup _
          | Just tupType <- prj tup = case tupType of
              -- Tup2_t -> VTup2 (sugar (sel1 a)) (sugar (sel2 a))
              -- ...
              Tup2_t  -> $(applyN 2  (ConE 'VTup2)  (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup3_t  -> $(applyN 3  (ConE 'VTup3)  (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup4_t  -> $(applyN 4  (ConE 'VTup4)  (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup5_t  -> $(applyN 5  (ConE 'VTup5)  (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup6_t  -> $(applyN 6  (ConE 'VTup6)  (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup7_t  -> $(applyN 7  (ConE 'VTup7)  (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup8_t  -> $(applyN 8  (ConE 'VTup8)  (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup9_t  -> $(applyN 9  (ConE 'VTup9)  (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup10_t -> $(applyN 10 (ConE 'VTup10) (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup11_t -> $(applyN 11 (ConE 'VTup11) (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup12_t -> $(applyN 12 (ConE 'VTup12) (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup13_t -> $(applyN 13 (ConE 'VTup13) (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup14_t -> $(applyN 14 (ConE 'VTup14) (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
              Tup15_t -> $(applyN 15 (ConE 'VTup15) (\i -> AppE (VarE 'sugar) (AppE (VarE (mkName ("sel" ++ show i))) (VarE 'a))))
        go t _ _ | Right Dict <- pwit (Proxy :: Proxy pred) t = Actual $ sugar a



--------------------------------------------------------------------------------
-- * Operations
--------------------------------------------------------------------------------

-- | Map over a 'Virtual' structure
mapVirtual :: forall pred c1 c2 b
    .  (forall a . pred a => c1 a -> c2 a)
    -> Virtual pred c1 b
    -> Virtual pred c2 b
mapVirtual f = go
  where
    go :: Virtual pred c1 a -> Virtual pred c2 a
    go (Actual a) = Actual (f a)
    -- go tup = case tup of
    --   VTup2 a b -> VTup2 (go a) (go b)
    --   ...
    go tup =
      $( virtualCases 15 (VarE 'tup)
          (\w -> foldl AppE (ConE (mkName ("VTup" ++ show w)))
               . map (AppE (VarE (mkName "go")))
          )
       )

-- | Monadic map over a 'Virtual' structure
mapVirtualA :: forall m pred c1 c2 b . Applicative m
    => (forall a . pred a => c1 a -> m (c2 a))
    -> Virtual pred c1 b -> m (Virtual pred c2 b)
mapVirtualA f = go
  where
    go :: Virtual pred c1 a -> m (Virtual pred c2 a)
    go (Actual a) = Actual <$> (f a)
    -- go tup = case tup of
    --   VTup2 a b -> pure VTup2 <*> go a <*> go b
    --   ...
    go tup =
      $( virtualCases 15 (VarE 'tup)
          (\w -> foldl
                   (\a b -> foldl1 AppE [VarE '(<*>), a, b])
                   (AppE (VarE 'pure) (ConE (mkName ("VTup" ++ show w))))
               . map (AppE (VarE (mkName "go")))
          )
       )

-- | Map over a 'Virtual' structure
mapVirtualA_ :: forall m pred cont b . Applicative m =>
    (forall a . pred a => cont a -> m ()) -> Virtual pred cont b -> m ()
mapVirtualA_ f = go
  where
    go :: Virtual pred cont a -> m ()
    go (Actual a) = f a
    -- go tup = case tup of
    --   VTup2 a b -> go a *> go b
    --   ...
    go tup =
      $( virtualCases 15 (VarE 'tup)
          (\_ -> foldr1 (\a b -> foldl1 AppE [VarE '(*>), a, b])
               . map (AppE (VarE (mkName "go")))
          )
       )

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
    go (Actual a) = [f a]
    -- go tup = case tup of
    --   VTup2 a b -> go a ++ go b
    --   ...
    go tup =
      $( virtualCases 15 (VarE 'tup)
          (\_ -> foldr1 (\a b -> foldl1 AppE [VarE '(++), a, b])
               . map (AppE (VarE (mkName "go")))
          )
       )

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
    go (Actual a) (Actual b) = Actual (f a b)
    -- go tup1 tup2 = case (tup1,tup2) of
    --   (VTup2 a b, VTup2 c d) -> VTup2 (go a c) (go b d)
    --   ...
    go tup1 tup2 =
      $( virtualCases2 15 (VarE 'tup1, VarE 'tup2)
          ( \w (sub1,sub2)
              -> foldl AppE (ConE (mkName ("VTup" ++ show w)))
                   ( map (\(s1,s2) -> foldl1 AppE [VarE (mkName "go"), s1, s2])
                      (zip sub1 sub2)
                   )
          )
          Nothing
       )

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
    go (Actual a) (Actual b) = [f a b]
    -- go tup1 tup2 = case (tup1,tup2) of
    --   (VTup2 a b, VTup2 c d) -> go a c ++ go b d
    --   ...
    go tup1 tup2 =
      $( virtualCases2 15 (VarE 'tup1, VarE 'tup2)
          ( \w (sub1,sub2)
              -> foldr1 (\a b -> foldl1 AppE [VarE '(++), a, b])
                   ( map (\(s1,s2) -> foldl1 AppE [VarE (mkName "go"), s1, s2])
                      (zip sub1 sub2)
                   )
          )
          Nothing
       )

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
    go (Actual a) (Actual b) = f a b
    -- go tup1 tup2 = case (tup1,tup2) of
    --   (VTup2 a b, VTup2 c d) -> go a c && go b d
    --   ...
    go tup1 tup2 =
      $( virtualCases2 15 (VarE 'tup1, VarE 'tup2)
          ( \w (sub1,sub2)
              -> foldr1 (\a b -> foldl1 AppE [VarE '(&&), a, b])
                   ( map (\(s1,s2) -> foldl1 AppE [VarE (mkName "go"), s1, s2])
                      (zip sub1 sub2)
                   )
          )
          (Just (ConE 'False))
       )

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
