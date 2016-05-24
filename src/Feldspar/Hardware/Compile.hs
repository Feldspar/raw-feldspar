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

import qualified Language.Embedded.Hardware       as Hard
import Language.Embedded.Hardware.Command.Backend.VHDL

import qualified Language.Embedded.Imperative     as Soft
import qualified Language.Embedded.Imperative.CMD as Soft
import qualified Language.Embedded.Expression     as Soft

import Data.TypedStruct
import Feldspar.Primitive.Representation
import Feldspar.Primitive.Backend.VHDL ()
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

translateExp :: forall m a. Monad m => HData a -> TargetT m (VExp a)
translateExp = goAST {-. optimize-} . unHData
  where
    goAST :: ASTF HFeldDomain b -> TargetT m (VExp b)
    goAST = simpleMatch (\(s :&: ValT t) -> go t s)

    goSmallAST :: PrimType' b => ASTF HFeldDomain b -> TargetT m (Prim b)
    goSmallAST = fmap extractSingle . goAST

    go :: TypeRep (DenResult sig)
       -> HFeldConstructs sig
       -> Args (AST HFeldDomain) sig
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
{-
    go t unsafe Nil
      | Just (UnsafePerform prog) <- prj unsafe
      = translateExp =<< Oper.reexpressEnv unsafeTranslateSmallExp (Oper.liftProgram $ unComp prog)
    go t unsafe (a :* Nil)
      | Just (UnsafePerformWith prog) <- prj unsafe
      = do a' <- goAST a
           Oper.reexpressEnv unsafeTranslateSmallExp (Oper.liftProgram $ unComp prog)
           return a'
-}
    go _ s _ = error $ renderSym s

unsafeTranslateSmallExp :: Monad m => HData a -> TargetT m (Prim a)
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

-- | ...
runIO :: MonadHardware m => m a -> IO a
runIO = Hard.runIO . translate . liftHardware

-- | Interpret a program in the 'IO' monad.
--
-- See ../Run/runIO'.
runIO' :: MonadHardware m => m a -> IO a
runIO'
    = Oper.interpretWithMonadBiT
        (return . Hard.evalE)
        Oper.interpBi
        (Oper.interpretBi (return . Hard.evalE))
    . unHardware
    . liftHardware

-- | Compile a program to VHDL code represented as a string.
compile :: MonadHardware m => m a -> String
compile = Hard.compile . translate . liftHardware

-- | ...
icompile :: MonadHardware m => m a -> IO ()
icompile = putStrLn . compile

{-
icompile :: (MonadHardware m, Harden a) => m a -> IO ()
icompile = Hard.wcompile . fmap (const ()) . lowerTop . liftHardware
-}

--------------------------------------------------------------------------------

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
softenArr (Hard.VArrayE v) = Soft.ArrRun v

hardenArr :: Soft.Arr i a -> Hard.VArray i a
hardenArr (Soft.ArrComp i) = Hard.VArrayC i
hardenArr (Soft.ArrRun v)  = Hard.VArrayE v

softenIArr :: Hard.IArray i a -> Soft.IArr i a
softenIArr (Hard.IArrayC i) = Soft.IArrComp i
softenIArr (Hard.IArrayE v) = Soft.IArrRun v

hardenIArr :: Soft.IArr i a -> Hard.IArray i a
hardenIArr (Soft.IArrComp i) = Hard.IArrayC i
hardenIArr (Soft.IArrRun v)  = Hard.IArrayE v

--------------------------------------------------------------------------------
