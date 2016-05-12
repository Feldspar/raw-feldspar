{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Hardware.Compile where



import Control.Monad.Identity
import Control.Monad.Reader
import Data.Constraint (Dict (..))
import Data.Map (Map)
import qualified Data.Map   as Map
import qualified Data.IORef as IORef 


import Language.Syntactic hiding ((:+:) (..), (:<:) (..))
import Language.Syntactic.Functional hiding (Binding (..))
import Language.Syntactic.Functional.Tuple

import Data.ALaCarte (Param0, Param1, Param2, Param3)
import Control.Monad.Operational.Higher (Program, ProgramT, (:+:))
import qualified Control.Monad.Operational.Higher as Oper

import qualified Language.Embedded.Hardware as Hard
import qualified Language.Embedded.Imperative     as Soft
import qualified Language.Embedded.Imperative.CMD as Soft
import qualified Language.Embedded.Expression     as Soft

import Data.TypedStruct
import Feldspar.Primitive.Representation
import Feldspar.Primitive.Backend.C ()
import Feldspar.Representation
import Feldspar.Hardware.Representation
import Feldspar.Optimize



--------------------------------------------------------------------------------
-- * Struct expressions and variables
--------------------------------------------------------------------------------

-- | Struct expression.
type VExp = Struct PrimType' Prim

-- | Struct expression with hidden result type.
data VExp'
  where
    VExp' :: Struct PrimType' Prim a -> VExp'

newRefV :: Monad m => TypeRep a -> String -> TargetT m (Struct PrimType' Hard.Variable a)
newRefV t base = lift $ mapStructA (const (Hard.newNamedVariable base)) t

initRefV :: Monad m => String -> VExp a -> TargetT m (Struct PrimType' Hard.Variable a)
initRefV base = lift . mapStructA (Hard.initNamedVariable base)

getRefV :: Monad m => Struct PrimType' Hard.Variable a -> TargetT m (VExp a)
getRefV = lift . mapStructA Hard.getVariable

setRefV :: Monad m => Struct PrimType' Hard.Variable a -> VExp a -> TargetT m ()
setRefV r = lift . sequence_ . zipListStruct Hard.setVariable r

unsafeFreezeRefV :: Monad m => Struct PrimType' Hard.Variable a -> TargetT m (VExp a)
unsafeFreezeRefV = lift . mapStructA Hard.unsafeFreezeVariable


--------------------------------------------------------------------------------
-- * Translation environment.
--------------------------------------------------------------------------------

-- | Translation environment.
type Env = Map Name VExp'

-- | Add a local alias to the environment.
localAlias :: MonadReader Env m
    => Name    -- ^ Old name
    -> VExp a  -- ^ New expression
    -> m b
    -> m b
localAlias v e = local (Map.insert v (VExp' e))

-- | Lookup an alias in the environment.
lookAlias :: MonadReader Env m => TypeRep a -> Name -> m (VExp a)
lookAlias t v = do
    env <- ask
    return $ case Map.lookup v env of
        Nothing -> error $ "lookAlias: variable " ++ show v ++ " not in scope"
        Just (VExp' e) -> case typeEq t (toTypeRep e) of Just Dict -> e



--------------------------------------------------------------------------------
-- * Translation of expressions.
--------------------------------------------------------------------------------

type TargetCMD
    =   Hard.VariableCMD
    :+: Hard.VArrayCMD
    :+: Hard.LoopCMD
    :+: Hard.ConditionalCMD
    :+: Hard.StructuralCMD
    :+: Hard.SignalCMD
        -- Leftovers from reexpress translation of Comp.
    :+: Soft.RefCMD
    :+: Soft.ArrCMD
    :+: Soft.ControlCMD

-- | Target monad during translation.
type TargetT m = ReaderT Env (ProgramT TargetCMD (Param2 Prim PrimType') m)

-- | Monad for intermediate programs.
type ProgI = Program TargetCMD (Param2 Prim PrimType')

translateExp :: forall m a. Monad m => Data a -> TargetT m (VExp a)
translateExp = goAST . optimize . unData
  where
    goAST :: ASTF FeldDomain b -> TargetT m (VExp b)
    goAST = simpleMatch (\(s :&: ValT t) -> go t s)

    goSmallAST :: PrimType' b => ASTF FeldDomain b -> TargetT m (Prim b)
    goSmallAST = fmap extractSingle . goAST

    go :: TypeRep (DenResult sig)
       -> FeldConstructs sig
       -> Args (AST FeldDomain) sig
       -> TargetT m (VExp (DenResult sig))
    go t lit Nil
      | Just (Lit a) <- prj lit
      = return $ mapStruct (Hard.litE . runIdentity) $ toStruct t a
    go t lit Nil
      | Just (Literal a) <- prj lit
      = return $ mapStruct (Hard.litE . runIdentity) $ toStruct t a
    go t var Nil
      | Just (VarT v) <- prj var
      = lookAlias t v
    go t lt (a :* (lam :$ body) :* Nil)
      | Just (Let tag) <- prj lt
      , Just (LamT v)  <- prj lam
      = do let base = if null tag then "let" else tag
           r  <- initRefV base =<< goAST a
           a' <- unsafeFreezeRefV r
           localAlias v a' $ goAST body
    go t tup (a :* b :* Nil)
      | Just Pair <- prj tup
      = Two <$> goAST a <*> goAST b
    go t sel (ab :* Nil)
      | Just Fst <- prj sel
      = do Two a _ <- goAST ab
           return a
      | Just Snd <- prj sel
      = do Two _ b <- goAST ab
           return b
    go _ c Nil
      | Just Pi <- prj c = return $ Single $ sugarSymPrim Pi
    go _ op (a :* Nil)
      | Just Neg   <- prj op = liftStruct (sugarSymPrim Neg)   <$> goAST a
      | Just Sin   <- prj op = liftStruct (sugarSymPrim Sin)   <$> goAST a
      | Just Cos   <- prj op = liftStruct (sugarSymPrim Cos)   <$> goAST a
      | Just I2N   <- prj op = liftStruct (sugarSymPrim I2N)   <$> goAST a
      | Just I2B   <- prj op = liftStruct (sugarSymPrim I2B)   <$> goAST a
      | Just B2I   <- prj op = liftStruct (sugarSymPrim B2I)   <$> goAST a
      | Just Round <- prj op = liftStruct (sugarSymPrim Round) <$> goAST a
      | Just Not   <- prj op = liftStruct (sugarSymPrim Not)   <$> goAST a
    go _ op (a :* b :* Nil)
      | Just Add  <- prj op = liftStruct2 (sugarSymPrim Add)  <$> goAST a <*> goAST b
      | Just Sub  <- prj op = liftStruct2 (sugarSymPrim Sub)  <$> goAST a <*> goAST b
      | Just Mul  <- prj op = liftStruct2 (sugarSymPrim Mul)  <$> goAST a <*> goAST b
      | Just FDiv <- prj op = liftStruct2 (sugarSymPrim FDiv) <$> goAST a <*> goAST b
      | Just Quot <- prj op = liftStruct2 (sugarSymPrim Quot) <$> goAST a <*> goAST b
      | Just Rem  <- prj op = liftStruct2 (sugarSymPrim Rem)  <$> goAST a <*> goAST b
      | Just Pow  <- prj op = liftStruct2 (sugarSymPrim Pow)  <$> goAST a <*> goAST b
      | Just Eq   <- prj op = liftStruct2 (sugarSymPrim Eq)   <$> goAST a <*> goAST b
      | Just And  <- prj op = liftStruct2 (sugarSymPrim And)  <$> goAST a <*> goAST b
      | Just Or   <- prj op = liftStruct2 (sugarSymPrim Or)   <$> goAST a <*> goAST b
      | Just Lt   <- prj op = liftStruct2 (sugarSymPrim Lt)   <$> goAST a <*> goAST b
      | Just Gt   <- prj op = liftStruct2 (sugarSymPrim Gt)   <$> goAST a <*> goAST b
      | Just Le   <- prj op = liftStruct2 (sugarSymPrim Le)   <$> goAST a <*> goAST b
      | Just Ge   <- prj op = liftStruct2 (sugarSymPrim Ge)   <$> goAST a <*> goAST b
    go (Single _) arrIx (i :* Nil)
      | Just (ArrIx arr) <- prj arrIx
      = do i' <- goSmallAST i
           return $ Single $ sugarSymPrim (ArrIx arr) i'
    go t cond (c :* tru :* fls :* Nil)
      | Just Cond <- prj cond
      = do env <- ask
           case (runReaderT (goAST tru) env, runReaderT (goAST fls) env) of
             (t', f') ->
               do tView <- lift $ lift $ Oper.viewT t'
                  fView <- lift $ lift $ Oper.viewT f'
                  case (tView, fView) of
                    (Oper.Return (Single tExp), Oper.Return (Single fExp)) ->
                      do c' <- goSmallAST c
                         return $ Single $ sugarSymPrim Cond c' tExp fExp
                    _ ->
                      do c'  <- goSmallAST c
                         res <- newRefV t "v"
                         ReaderT $ \env' -> Hard.iff c'
                           (flip runReaderT env' . setRefV res =<< t')
                           (flip runReaderT env' . setRefV res =<< f')
                         unsafeFreezeRefV res
    go t loop (len :* init :* (lami :$ (lams :$ body)) :* Nil)
      | Just ForLoop   <- prj loop
      , Just (LamT iv) <- prj lami
      , Just (LamT sv) <- prj lams
      = do len'  <- goSmallAST len
           state <- initRefV "state" =<< goAST init
           ReaderT $ \env -> Hard.for len' $ \i -> flip runReaderT env $
             do s  <- case t of
                  Single _ -> unsafeFreezeRefV state
                  _        -> getRefV state
                s' <- localAlias iv (Single i) $ localAlias sv s $ goAST body
                setRefV state s'
           unsafeFreezeRefV state
    go (Single _) free Nil
      | Just (FreeVar v) <- prj free
      = return $ Single $ sugarSymPrim $ FreeVar v
    go t unsafe Nil
      | Just (UnsafePerform prog) <- prj unsafe
      = translateExp =<< Oper.reexpressEnv unsafeTranslateSmallExp (Oper.liftProgram $ unComp prog)
    go t unsafe (a :* Nil)
      | Just (UnsafePerformWith prog) <- prj unsafe
      = do a' <- goAST a
           Oper.reexpressEnv unsafeTranslateSmallExp (Oper.liftProgram $ unComp prog)
           return a'
    go _ s _ = error $ renderSym s

unsafeTranslateSmallExp :: Monad m => Data a -> TargetT m (Prim a)
unsafeTranslateSmallExp a =
  do Single b <- translateExp a
     return b



--------------------------------------------------------------------------------
-- * Translation of instructions.
--------------------------------------------------------------------------------

type FinalCMD
    =   Hard.VariableCMD
    :+: Hard.VArrayCMD
    :+: Hard.LoopCMD
    :+: Hard.ConditionalCMD
    :+: Hard.StructuralCMD
    :+: Hard.SignalCMD

-- | Target monad during translation.
type FinalT m = ReaderT Env (ProgramT FinalCMD (Param2 Prim PrimType') m)

-- | Monad for intermediate programs.
type ProgH = Program FinalCMD (Param2 Prim PrimType')

class Lower instr
  where
    lowerInstr :: instr (Param3 ProgH Prim PrimType') a -> ProgH a

instance (Lower i, Lower j) => Lower (i :+: j)
  where
    lowerInstr (Oper.Inl i) = lowerInstr i
    lowerInstr (Oper.Inr j) = lowerInstr j

instance Lower Soft.RefCMD
  where
    lowerInstr (Soft.NewRef n)    = fmap softenRef $ Hard.newNamedVariable  n
    lowerInstr (Soft.InitRef n e) = fmap softenRef $ Hard.initNamedVariable n e
    lowerInstr (Soft.GetRef r)    = fmap softenVal $ Oper.singleInj $ Hard.GetVariable (hardenRef r)
    lowerInstr (Soft.SetRef r e)  = Hard.setVariable (hardenRef r) e
    lowerInstr (Soft.UnsafeFreezeRef r) = fmap softenVal $ Oper.singleInj $ Hard.UnsafeFreezeVariable (hardenRef r)

instance Lower Soft.ArrCMD
  where
    lowerInstr (Soft.NewArr n e)    = fmap softenArr $ Oper.singleInj $ Hard.NewVArray  (Hard.Base n) e
    lowerInstr (Soft.InitArr n es)  = fmap softenArr $ Oper.singleInj $ Hard.InitVArray (Hard.Base n) es
    lowerInstr (Soft.GetArr i a)    = fmap softenVal $ Oper.singleInj $ Hard.GetVArray i (hardenArr a)
    lowerInstr (Soft.SetArr i e a)  = Oper.singleInj $ Hard.SetVArray i e (hardenArr a)
    lowerInstr (Soft.CopyArr a b l) = Oper.singleInj $ Hard.CopyVArray (hardenArr a) (hardenArr b) l
    lowerInstr (Soft.UnsafeFreezeArr a) = fmap softenIArr $ Oper.singleInj $ Hard.UnsafeFreezeVArray (hardenArr a)
    lowerInstr (Soft.UnsafeThawArr ia)  = fmap softenArr $ Oper.singleInj $ Hard.UnsafeThawVArray (hardenIArr ia)

instance Lower Soft.ControlCMD
  where
    lowerInstr (Soft.If b tru fls)       = Oper.singleInj $ Hard.If (b, tru) [] (Just fls)
    lowerInstr (Soft.While cont body)    = Oper.singleInj $ Hard.While cont body
      -- this one is a temp. solution
    lowerInstr (Soft.For (_, _, b) body) = Oper.singleInj $ Hard.For (Soft.borderVal b) (body . softenVal)
    lowerInstr (Soft.Break)              = error "lower break"
    lowerInstr (Soft.Assert cond msg)    = error "lower assert"

instance Lower Hard.VariableCMD    where lowerInstr = Oper.singleInj
instance Lower Hard.VArrayCMD      where lowerInstr = Oper.singleInj
instance Lower Hard.StructuralCMD  where lowerInstr = Oper.singleInj
instance Lower Hard.SignalCMD      where lowerInstr = Oper.singleInj
instance Lower Hard.LoopCMD        where lowerInstr = Oper.singleInj
instance Lower Hard.ConditionalCMD where lowerInstr = Oper.singleInj

-- | ...
translate :: Hardware a -> ProgH a
translate = Oper.interpretWithMonad lowerInstr
          . Oper.interpretWithMonadT Oper.singleton id
          . flip runReaderT Map.empty . Oper.reexpressEnv unsafeTranslateSmallExp
          . Oper.interpretWithMonadT Oper.singleton
              (lift . flip runReaderT Map.empty . Oper.reexpressEnv unsafeTranslateSmallExp)
          . unHardware

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

runIO :: MonadHardware m => m a -> IO a
runIO = Hard.runIO . translate . liftHardware

-- | Interpret a program in the 'IO' monad
runIO' :: MonadHardware m => m a -> IO a
runIO'
    = Oper.interpretWithMonadBiT
        (return . Hard.evalE)
        Oper.interpBi
        (Oper.interpretBi (return . Hard.evalE))
    . unHardware
    . liftHardware
  -- Unlike `runIO`, this function does the interpretation directly, without
  -- first lowering the program. This might be faster, but I haven't done any
  -- measurements to se if it is.
  --
  -- One disadvantage with `runIO'` is that it cannot handle expressions
  -- involving `IOSym`. But at the moment of writing this, we're not using those
  -- symbols for anything anyway.

-- | Compile a program to VHDL code represented as a string.
compile :: MonadHardware m => m a -> String
compile = undefined --Hard.compile . translate . liftHardware

icompile :: MonadHardware m => m a -> IO ()
icompile = undefined

--------------------------------------------------------------------------------
-- * Target language.
--------------------------------------------------------------------------------
{-
type VExp = Virtual SmallType Hard.HExp

data VExp_ where
  VExp_ :: Type a => VExp a -> VExp_

type TargetCMD =
        Hard.SignalCMD      Hard.HExp
  H.:+: Hard.VariableCMD    Hard.HExp
  H.:+: Hard.ArrayCMD       Hard.HExp
  H.:+: Hard.LoopCMD        Hard.HExp
  H.:+: Hard.ConditionalCMD Hard.HExp
  H.:+: Hard.StructuralCMD  Hard.HExp

type Env = Map Name VExp_

-- | Target monad for translation
type Target = ReaderT Env (H.Program TargetCMD)

--------------------------------------------------------------------------------

newRefV :: VirtualType SmallType a => Target (Virtual SmallType Hard.Variable a)
newRefV = Reader.lift $ mapVirtualA (const Hard.newVariable) virtRep

initRefV :: VirtualType SmallType a => VExp a -> Target (Virtual SmallType Hard.Variable a)
initRefV = Reader.lift . mapVirtualA (Hard.initVariable)

getRefV :: VirtualType SmallType a => Virtual SmallType Hard.Variable a -> Target (VExp a)
getRefV = Reader.lift . mapVirtualA (Hard.getVariable)

setRefV :: VirtualType SmallType a => Virtual SmallType Hard.Variable a -> VExp a -> Target ()
setRefV r = Reader.lift . sequence_ . zipListVirtual (Hard.setVariable) r

unsafeFreezeRefV :: VirtualType SmallType a => Virtual SmallType Hard.Variable a -> Target (VExp a)
unsafeFreezeRefV = Reader.lift . mapVirtualA Hard.unsafeFreezeVariable

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

class lower instr
  where
    lowerInstr :: instr Target a -> Target a

instance Lower (Soft.RefCMD Data)
  where
    lowerInstr (Soft.NewRef base) =
      Reader.lift $ fmap softenRef $ Hard.newNamedVariable base
        -- Ignoring base name because there are no named references in
        -- hardware-edsl (same for `InitRef`, `NewArr` and `InitArr`)
    lowerInstr (Soft.InitRef base a) =
      Reader.lift . fmap softenRef . Hard.initNamedVariable base =<< translateSmallExp a
    lowerInstr (Soft.SetRef r a) =
      Reader.lift . Hard.setVariable (hardenRef r) =<< translateSmallExp a
    lowerInstr (Soft.GetRef r)   =
      fmap liftVar $ Reader.lift $ Hard.getVariable $ hardenRef r
    lowerInstr (Soft.UnsafeFreezeRef r) =
      fmap liftVar $ Reader.lift $ Hard.unsafeFreezeVariable $ hardenRef r

instance Lower (Soft.ArrCMD Data)
  where
    lowerInstr (Soft.NewArr base i)    =
      Reader.lift . fmap softenArray . Hard.newNamedArray base =<< translateSmallExp i
    lowerInstr (Soft.InitArr base xs)  =
      Reader.lift $ fmap softenArray $ Hard.initNamedArray base xs
    lowerInstr (Soft.SetArr i v a)     = do
      i' <- translateSmallExp i
      v' <- translateSmallExp v
      Reader.lift $ Hard.setArray i' v' (hardenArray a)
    lowerInstr (Soft.GetArr i a)       = do
      i' <- translateSmallExp i
      fmap liftVar $ Reader.lift $ Hard.getArray i' (hardenArray a)
    lowerInstr (Soft.CopyArr dst src n) =
      Reader.lift . Hard.copyArray (hardenArray dst) (hardenArray src) =<< translateSmallExp n
    lowerInstr (Soft.UnsafeFreezeArr a) =
      Reader.lift $ fmap softenIArray $ Hard.unsafeFreezeArray (hardenArray a)

instance Lower (Soft.ControlCMD Data)
  where
    lowerInstr (Soft.If c t f) = do
        c' <- translateSmallExp c
        ReaderT $ \env -> Hard.iff c'
            (runReaderT t env)
            (runReaderT f env)
    lowerInstr (Soft.While cont body) = do
        ReaderT $ \env -> Hard.while
            (runReaderT (translateSmallExp =<< cont) env)
            (runReaderT body env)
    -- VHDL always uses a step length of one by default. For longer steps,
    -- we could to some enum tricks. For now I'll also assume lo is zero.
    lowerInstr (Soft.For (_, _, hi) body) = do
        hi' <- traverse translateSmallExp hi
        let l = case hi' of
                  (Soft.Incl i) -> i
                  (Soft.Excl i) -> i - 1
        ReaderT $ \env -> Hard.for l (flip runReaderT env . body . liftVar)
    lowerInstr (Soft.Assert {}) = error "lower-todo: assert?"
    lowerInstr (Soft.Break  {}) = error "lower-todo: break out of loops."

instance Lower (Hard.SignalCMD Data)
  where
    lowerInstr (Hard.NewSignal base _ _ _ Nothing) =
      Reader.lift $ Hard.newNamedSignal base
    lowerInstr (Hard.NewSignal base _ _ _ (Just e)) =
      Reader.lift . Hard.initNamedSignal base =<< translateSmallExp e
    lowerInstr (Hard.SetSignal s e) =
      Reader.lift . Hard.setSignal  s =<< translateSmallExp e
    lowerInstr (Hard.GetSignal s) =
      fmap liftVar $ Reader.lift $ Hard.getSignal s
    lowerInstr (Hard.UnsafeFreezeSignal s) =
      fmap liftVar $ Reader.lift $ Hard.unsafeFreezeSignal s

instance Lower (Hard.LoopCMD Data)
  where
    lowerInstr (Hard.For   cont body) = do
      cont' <- translateSmallExp cont
      ReaderT $ \env -> Hard.for
        (cont')
        (flip runReaderT env . body . liftVar)
    lowerInstr (Hard.While exit body) = do
      ReaderT $ \env -> Hard.while
        (runReaderT (translateSmallExp =<< exit) env)
        (runReaderT body env)

instance Lower (Hard.ConditionalCMD Data)
  where
    lowerInstr (Hard.If (c, i) t e) = do
      let (bs, ps) = unzip t
      c'  <-      translateSmallExp c
      bs' <- mapM translateSmallExp bs
      ReaderT $ \env -> Hard.conditional
        (c', runReaderT i env)
        (zip bs' $ fmap (flip runReaderT env) ps)
        (maybe Nothing (Just . flip runReaderT env) e)

instance Lower (Hard.StructuralCMD Data)
  where
    lowerInstr (Hard.StructEntity e p) = do
      ReaderT $ \env -> Hard.structEntity e (runReaderT p env)
    lowerInstr (Hard.StructArchitecture e a p) = do
      ReaderT $ \env -> Hard.structArchitecture e a (runReaderT p env)
    lowerInstr (Hard.StructProcess ss p) = do
      ReaderT $ \env -> Hard.structProcess ss (runReaderT p env)

instance (Lower i, Lower j) => Lower (i H.:+: j)
  where
    lowerInstr (H.Inl i) = lowerInstr i
    lowerInstr (H.Inr j) = lowerInstr j

-- | Lift a 'HExp' that has been created using 'Hard.litE' or 'Hard.varE'.
liftVar :: SmallType a => Hard.HExp a -> Data a
liftVar (Hard.HExp (Sym (Hard.T dom)))
  | Just (Hard.Name n)    <- prj dom = Data $ Sym $ inj (FreeVar n) :&: typeRep
  | Just (Hard.Literal l) <- prj dom = Feld.value l

-- | Transforms a software reference into a hardware variable.
hardenRef :: SmallType a => Soft.Ref a -> Hard.Variable a
hardenRef (Soft.RefComp ('r':i)) = Hard.VariableC (read i)
hardenRef (Soft.RefEval i)       = Hard.VariableE i

softenRef :: SmallType a => Hard.Variable a -> Soft.Ref a
softenRef (Hard.VariableC i) = Soft.RefComp ('r' : show i)
softenRef (Hard.VariableE i) = Soft.RefEval i

-- | Transforms a software array into a hardware one.
hardenArray :: SmallType a => Soft.Arr i a -> Hard.Array i a
hardenArray (Soft.ArrComp i) = Hard.ArrayC i
hardenArray (Soft.ArrEval i) = Hard.ArrayE i

softenArray :: SmallType a => Hard.Array i a -> Soft.Arr i a
softenArray (Hard.ArrayC i) = Soft.ArrComp ('a' : show i)
softenArray (Hard.ArrayE i) = Soft.ArrEval i

softenIArray :: SmallType a => Hard.IArray i a -> Soft.IArr i a
softenIArray (Hard.IArrayC i) = Soft.IArrComp ('a' : show i)
softenIArray (Hard.IArrayE i) = Soft.IArrEval i

--------------------------------------------------------------------------------

type family HW a
type instance HW (Soft.Ref   a) = Hard.Variable a
type instance HW (Soft.Arr i a) = Hard.Array i a
type instance HW (Data a)       = Data a
type instance HW ()             = ()

class Harden a where
  harden :: a -> HW a

instance SmallType a => Harden (Soft.Ref a)   where harden = hardenRef
instance SmallType a => Harden (Soft.Arr i a) where harden = hardenArray
instance SmallType a => Harden (Data a)       where harden = id
instance                Harden ()             where harden = id

-- | Translate a Feldspar program to the 'Target' monad.
lower :: (Lower instr, H.HFunctor instr, Harden a) => H.Program instr a -> Target (HW a)
lower = fmap harden . H.interpretWithMonad lowerInstr

-- | Translate a Hardware program into a program that uses 'TargetCMD'.
lowerTop :: Harden a => Hardware a -> H.Program TargetCMD (HW a)
lowerTop = flip runReaderT Map.empty . fmap harden . interp2 . unHardware
  where
    interp2 :: (H.HFunctor i, Lower i, H.HFunctor j, Lower j) => H.ProgramT i (H.Program j) a -> Target a
    interp2 = H.interpretWithMonadT lowerInstr (H.interpretWithMonad lowerInstr)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | ...
translateAST :: ASTF FeldDomain a -> Target (VExp a)
translateAST = goAST . optimize
  where
    goAST :: ASTF FeldDomain a -> Target (VExp a)
    goAST = simpleMatch (\(s :&: t) -> go t s)

    goSmallAST :: SmallType a => ASTF FeldDomain a -> Target (Hard.HExp a)
    goSmallAST = fmap viewActual . goAST

    go :: TypeRep FeldTypes (DenResult sig)
       -> FeldConstructs sig
       -> Args (AST FeldDomain) sig
       -> Target (VExp (DenResult sig))
    go t lit Nil
        | Just (Literal a) <- prj lit
        , Right Dict <- pwit pType t
        = return $ mapVirtual (Hard.value . runIdentity) $ toVirtual a
    go t var Nil
        | Just (VarT v) <- prj var
        , Right Dict <- pwit pType t
        = lookAlias v
    go t lt (a :* (lam :$ body) :* Nil)
        | Just (Let _)  <- prj lt
            -- Ingoring tag because there are no named references in hardware-edsl
        , Just (LamT v) <- prj lam
        , Right Dict    <- pwit pType (getDecor a)
        = do r  <- initRefV =<< goAST a
             a' <- getRefV r
             localAlias v a' $ goAST body
    go t tup (a :* b :* Nil)
        | Just Tup2 <- prj tup = VTup2 <$> goAST a <*> goAST b
    go t tup (a :* b :* c :* Nil)
        | Just Tup3 <- prj tup = VTup3 <$> goAST a <*> goAST b <*> goAST c
    go t tup (a :* b :* c :* d :* Nil)
        | Just Tup4 <- prj tup = VTup4 <$> goAST a <*> goAST b <*> goAST c <*> goAST d
    go t sel (a :* Nil)
        | Just Sel1  <- prj sel = fmap vsel1  $ goAST a
        | Just Sel2  <- prj sel = fmap vsel2  $ goAST a
        | Just Sel3  <- prj sel = fmap vsel3  $ goAST a
        | Just Sel4  <- prj sel = fmap vsel4  $ goAST a
        | Just Sel5  <- prj sel = fmap vsel5  $ goAST a
        | Just Sel6  <- prj sel = fmap vsel6  $ goAST a
        | Just Sel7  <- prj sel = fmap vsel7  $ goAST a
        | Just Sel8  <- prj sel = fmap vsel8  $ goAST a
        | Just Sel9  <- prj sel = fmap vsel9  $ goAST a
        | Just Sel10 <- prj sel = fmap vsel10 $ goAST a
        | Just Sel11 <- prj sel = fmap vsel11 $ goAST a
        | Just Sel12 <- prj sel = fmap vsel12 $ goAST a
        | Just Sel13 <- prj sel = fmap vsel13 $ goAST a
        | Just Sel14 <- prj sel = fmap vsel14 $ goAST a
        | Just Sel15 <- prj sel = fmap vsel15 $ goAST a
    go t op (a :* Nil)
        | Just I2N <- prj op = error "sup I2N"
        | Just Not <- prj op = liftVirt (Hard.not) <$> goAST a
    go t op (a :* b :* Nil)
        | Just Add <- prj op = liftVirt2 (+)        <$> goAST a <*> goAST b
        | Just Sub <- prj op = liftVirt2 (-)        <$> goAST a <*> goAST b
        | Just Mul <- prj op = liftVirt2 (*)        <$> goAST a <*> goAST b
        | Just Eq  <- prj op = liftVirt2 (Hard.eq)  <$> goAST a <*> goAST b
        | Just Lt  <- prj op = liftVirt2 (Hard.lt)  <$> goAST a <*> goAST b
        | Just Gt  <- prj op = liftVirt2 (Hard.gt)  <$> goAST a <*> goAST b
        | Just Le  <- prj op = liftVirt2 (Hard.lte) <$> goAST a <*> goAST b
        | Just Ge  <- prj op = liftVirt2 (Hard.gte) <$> goAST a <*> goAST b
    go t arrIx (i :* Nil)
        | Just (ArrIx arr) <- prj arrIx = error "translateAST-todo: array indexing in expressions"
    go ty cond (c :* t :* f :* Nil)
        | Just Condition <- prj cond = do
            env <- Reader.ask
            case () of
              _ | H.Return (Actual t') <- H.view $ flip runReaderT env $ goAST t
                , H.Return (Actual f') <- H.view $ flip runReaderT env $ goAST f
                -> do
                    c' <- goSmallAST c
                    return $ Actual (error "inline if") --((?) c' t' f')
              _ -> do
                    c'  <- goSmallAST c
                    res <- newRefV
                    ReaderT $ \env -> Hard.iff c'
                        (flip runReaderT env $ goAST t >>= setRefV res)
                        (flip runReaderT env $ goAST f >>= setRefV res)
                    getRefV res
    go t loop (len :* init :* (lami :$ (lams :$ body)) :* Nil)
        | Just ForLoop   <- prj loop
        , Just (LamT iv) <- prj lami
        , Just (LamT sv) <- prj lams
        = do len'  <- goSmallAST len
             state <- initRefV =<< goAST init
             ReaderT $ \env -> Hard.for (len' - 1) $ \i -> flip runReaderT env $ do
                s <- case pwit pSmallType t of
                    Right Dict -> getRefV state  -- For non-compound states
                    _          -> getRefV state
                s' <- localAlias iv (Actual i) $
                        localAlias sv s $
                          goAST body
                setRefV state s'
             getRefV state
    go t free Nil
        | Just (FreeVar v) <- prj free
        = return $ Actual $ Hard.variable v
{-
    go t unsPerf Nil
        | Just (UnsafePerform prog) <- prj unsPerf
        = translateExp =<< lower (unComp prog)
-}
    go t unsPerf (a :* Nil)
        | Just (UnsafePerformWith prog) <- prj unsPerf = do
            a' <- goAST a
            lower (unComp prog)
            return a'

-- | Add a local alias to the environment
localAlias :: Type a
    => Name    -- ^ Old name
    -> VExp a  -- ^ New expression
    -> Target b
    -> Target b
localAlias v e = Reader.local (Map.insert v (VExp_ e))

-- | Lookup an alias in the environment
lookAlias :: forall a . Type a => Name -> Target (VExp a)
lookAlias v = do
    env <- Reader.ask
    return $ case Map.lookup v env of
        Nothing | Right Dict <- pwit pHType tr
               -> error $ "lookAlias: variable " ++ show v ++ " not in scope"
        Just (VExp_ e) -> case gcast pFeldTypes e of
            Left msg -> error $ "lookAlias: " ++ msg
            Right e' -> e'
  where
    tr = typeRep :: TypeRep FeldTypes a

--------------------------------------------------------------------------------

-- | Translate a Feldspar expression.
translateExp :: Data a -> Target (VExp a)
translateExp = translateAST . unData

-- | Translate a Feldspar expression that can be represented as a simple 'HExp'.
translateSmallExp :: SmallType a => Data a -> Target (Hard.HExp a)
translateSmallExp = fmap viewActual . translateExp

--------------------------------------------------------------------------------
-- * Run.
--------------------------------------------------------------------------------

-- | Interpret a program in the 'IO' monad.
runIO :: (MonadHardware m, Harden a) => m a -> IO (HW a)
runIO = H.interpret . lowerTop . liftHardware

-- | Compile a program to VHDL code represented as a string.
compile :: (MonadHardware m, Harden a) => m a -> String
compile = Hard.compile . lowerTop . liftHardware

-- | Compile a program to VHDL code and print it on the screen.
icompile :: (MonadHardware m, Harden a) => m a -> IO ()
icompile = Hard.wcompile . fmap (const ()) . lowerTop . liftHardware

--------------------------------------------------------------------------------
-- Stuff
--------------------------------------------------------------------------------

pHType :: Proxy HType
pHType = Proxy

instance ShowClass HType where showClass _ = "HType"

deriveWitness ''HType ''BoolType
deriveWitness ''HType ''FloatType
deriveWitness ''HType ''DoubleType
deriveWitness ''HType ''IntWordType

derivePWitness ''HType ''BoolType
derivePWitness ''HType ''FloatType
derivePWitness ''HType ''DoubleType
derivePWitness ''HType ''IntWordType

instance PWitness HType CharType t
instance PWitness HType ListType t
instance PWitness HType TupleType t
instance PWitness HType FunType t

-- ...

--------------------------------------------------------------------------------

-}

softenVal :: Hard.Val a -> Soft.Val a
softenVal (Hard.ValC i) = Soft.ValComp i
softenVal (Hard.ValE e) = Soft.ValRun e

hardenVal :: Soft.Val a -> Hard.Val a
hardenVal (Soft.ValComp i) = Hard.ValC i
hardenVal (Soft.ValRun e)  = Hard.ValE e

softenRef :: Hard.Variable a -> Soft.Ref a
softenRef (Hard.VariableC i) = Soft.RefComp i
softenRef (Hard.VariableE v) = Soft.RefRun v
 
hardenRef :: Soft.Ref a -> Hard.Variable a
hardenRef (Soft.RefComp i) = Hard.VariableC i
hardenRef (Soft.RefRun v)  = Hard.VariableE v

softenArr :: Hard.VArray i a -> Soft.Arr i a
softenArr (Hard.VArrayC i) = Soft.ArrComp i
softenArr (Hard.VArrayE v) = Soft.ArrRun (error "IO?!") -- (newIORef v)

hardenArr :: Soft.Arr i a -> Hard.VArray i a
hardenArr (Soft.ArrComp i) = Hard.VArrayC i
hardenArr (Soft.ArrRun v)  = Hard.VArrayE (error "IO?!")

softenIArr :: Hard.IArray i a -> Soft.IArr i a
softenIArr (Hard.IArrayC i) = Soft.IArrComp i
softenIArr (Hard.IArrayE v) = Soft.IArrRun v

hardenIArr :: Soft.IArr i a -> Hard.IArray i a
hardenIArr (Soft.IArrComp i) = Hard.IArrayC i
hardenIArr (Soft.IArrRun v)  = Hard.IArrayE v

--------------------------------------------------------------------------------
