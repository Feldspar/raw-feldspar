{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Compile.Hardware where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Identity (runIdentity)
import qualified Control.Monad.Reader as Reader

import qualified Control.Monad.Operational.Higher as H

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Language.Syntactic hiding ((:+:) (..), (:<:) (..))
import Language.Syntactic.Functional hiding (Binding (..))
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.TH

import Data.TypeRep
import Data.TypeRep.TH
import Data.TypeRep.Types.Basic
import Data.TypeRep.Types.Tuple
import Data.TypeRep.Types.IntWord

import Language.Embedded.Hardware (HType)
import qualified Language.Embedded.Hardware                   as Hard
import qualified Language.Embedded.Hardware.Expression.Syntax as Hard

--------------------------------------------------------------------------------
import qualified Language.Embedded.VHDL as VHDL
--------------------------------------------------------------------------------

import qualified Language.Embedded.Imperative     as Soft
import qualified Language.Embedded.Imperative.CMD as Soft

import Data.VirtualContainer

import Feldspar.Representation hiding (Program)
import Feldspar.Optimize
import Feldspar.Compile.Lower
import qualified Feldspar.Representation as Feld
import qualified Feldspar.Frontend       as Feld

--------------------------------------------------------------------------------
-- * Target language.
--------------------------------------------------------------------------------

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
newRefV = Reader.lift $ mapVirtualA (const Hard.newVariable_) virtRep

initRefV :: VirtualType SmallType a => VExp a -> Target (Virtual SmallType Hard.Variable a)
initRefV = Reader.lift . mapVirtualA (Hard.newVariable)

getRefV :: VirtualType SmallType a => Virtual SmallType Hard.Variable a -> Target (VExp a)
getRefV = Reader.lift . mapVirtualA (Hard.getVariable)

setRefV :: VirtualType SmallType a => Virtual SmallType Hard.Variable a -> VExp a -> Target ()
setRefV r = Reader.lift . sequence_ . zipListVirtual (Hard.setVariable) r

unsafeFreezeRefV :: VirtualType SmallType a => Virtual SmallType Hard.Variable a -> Target (VExp a)
unsafeFreezeRefV = Reader.lift . mapVirtualA Hard.unsafeFreezeVariable

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

class Lower instr
  where
    lowerInstr :: instr Target a -> Target a

instance Lower (Soft.RefCMD Data)
  where
    lowerInstr (Soft.NewRef)     =
      Reader.lift $ fmap softenRef Hard.newVariable_
    lowerInstr (Soft.InitRef a)  =
      Reader.lift . fmap softenRef . Hard.newVariable =<< translateSmallExp a
    lowerInstr (Soft.SetRef r a) =
      Reader.lift . Hard.setVariable (hardenRef r) =<< translateSmallExp a
    lowerInstr (Soft.GetRef r)   =
      fmap liftVar $ Reader.lift $ Hard.getVariable $ hardenRef r
    lowerInstr (Soft.UnsafeFreezeRef r) =
      fmap liftVar $ Reader.lift $ Hard.unsafeFreezeVariable $ hardenRef r

instance Lower (Soft.ArrCMD Data)
  where
    lowerInstr (Soft.NewArr i)         =
      Reader.lift . fmap softenArray . Hard.newArray =<< translateSmallExp i
    lowerInstr (Soft.InitArr xs)       =
      Reader.lift $ fmap softenArray $ Hard.initArray xs
    lowerInstr (Soft.SetArr i v a)     = do
      i' <- translateSmallExp i
      v' <- translateSmallExp v
      Reader.lift $ Hard.setArray i' v' (hardenArray a)
    lowerInstr (Soft.GetArr i a)       = do
      i' <- translateSmallExp i
      fmap liftVar $ Reader.lift $ Hard.getArray i' (hardenArray a)
    lowerInstr (Soft.UnsafeGetArr i a) = do
      i' <- translateSmallExp i
      fmap liftVar $ Reader.lift $ Hard.unsafeGetArray i' (hardenArray a)
    lowerInstr (Soft.NewArr_)          = error "lower: hardware arrays must have a length."
    lowerInstr (Soft.CopyArr a b l)    = error "lower-todo: copy by selected-name assignment."

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
    lowerInstr (Soft.Assert {}) = error "lower: assert?"
    lowerInstr (Soft.Break  {}) = error "lower-todo: break out of loops."

instance Lower (Hard.SignalCMD Data)
  where
    lowerInstr (Hard.NewSignal _ _ _ Nothing) =
      Reader.lift $ Hard.newSignal_
    lowerInstr (Hard.NewSignal _ _ _ (Just e)) =
      Reader.lift . Hard.newSignal =<< translateSmallExp e
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
    lowerInstr (Hard.Entity e p) = do
      ReaderT $ \env -> Hard.entity e (runReaderT p env)
    lowerInstr (Hard.Architecture e a p) = do
      ReaderT $ \env -> Hard.architecture e a (runReaderT p env)
    lowerInstr (Hard.Process ss p) = do
      ReaderT $ \env -> Hard.process ss (runReaderT p env)

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
hardenRef (Soft.RefComp i) = Hard.VariableC i
hardenRef (Soft.RefEval i) = Hard.VariableE i

softenRef :: SmallType a => Hard.Variable a -> Soft.Ref a
softenRef (Hard.VariableC i) = Soft.RefComp i
softenRef (Hard.VariableE i) = Soft.RefEval i

-- | Transforms a software array into a hardware one.
hardenArray :: SmallType a => Soft.Arr i a -> Hard.Array i a
hardenArray (Soft.ArrComp ('a':i)) = Hard.ArrayC (read i :: Integer)
hardenArray (Soft.ArrEval i)       = Hard.ArrayE i

softenArray :: SmallType a => Hard.Array i a -> Soft.Arr i a
softenArray (Hard.ArrayC i) = Soft.ArrComp ('a' : show i)
softenArray (Hard.ArrayE i) = Soft.ArrEval i

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

-- | Translate a Feldspar program into a program that uses 'TargetCMD'.
lowerTop :: Harden a => Feld.Program a -> H.Program TargetCMD (HW a)
lowerTop = flip runReaderT Map.empty . lower . Feld.unProgram

-- | Translate a Hardware program into a program that uses 'TargetCMD'.
lowerHard :: Harden a => Feld.Hardware a -> H.Program TargetCMD (HW a)
lowerHard = flip runReaderT Map.empty . fmap harden . interp2 . unHardware
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
        | Just Let      <- prj lt
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
        | Just (FreeVar v) <- prj free = return $ Actual $ Hard.variable v
    go t arrIx (i :* Nil)
        | Just (UnsafeArrIx arr) <- prj arrIx = error "translateAST-todo: array indexing in expressions"
{-
    go t unsPerf Nil
        | Just (UnsafePerform prog) <- prj unsPerf
        = translateExp =<< lower (unProgram prog)
    go t unsPerf (a :* Nil)
        | Just (UnsafePerformWith prog) <- prj unsPerf = do
            a' <- goAST a
            lower (unProgram prog)
            return a'
-}

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
runIO :: Harden a => Feld.Program a -> IO (HW a)
runIO = H.interpret . lowerTop

-- | Compile a program to VHDL code represented as a string.
compile :: Harden a => Feld.Program a -> String
compile = Hard.compile . lowerTop

-- | Compile a program to VHDL code and print it on the screen.
icompile :: Harden a => Feld.Program a -> IO ()
icompile = putStrLn . compile

--------------------------------------------------------------------------------

-- | ...
compile2 :: Harden a => Feld.Hardware a -> String
compile2 = Hard.compile . lowerHard

-- | ...
icompile2 :: Harden a => Feld.Hardware a -> IO ()
icompile2 = putStrLn . compile2

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
