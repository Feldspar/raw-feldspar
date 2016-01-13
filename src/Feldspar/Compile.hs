{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Compile where

import Control.Applicative
import Data.Monoid
import Data.Traversable (traverse)

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Operational.Higher (interpretWithMonad)
import qualified Control.Monad.Operational.Higher as H

import Language.Syntactic hiding ((:+:) (..), (:<:) (..))
import Language.Syntactic.Functional hiding (Binding (..))
import Language.Syntactic.Functional.Tuple
import Language.Syntactic.TH

import Data.TypeRep
import Data.TypeRep.TH
import Data.TypeRep.Types.Basic
import Data.TypeRep.Types.Tuple
import Data.TypeRep.Types.IntWord

import Language.Embedded.VHDL (VType)
import qualified Language.Embedded.VHDL as Imp

import Data.VirtualContainer

import Feldspar.Representation hiding (Program)
import Feldspar.Optimize
import qualified Feldspar.Representation as Feld
import qualified Feldspar.Frontend as Feld

--------------------------------------------------------------------------------
-- * Virtual variables
--------------------------------------------------------------------------------

newRefV :: VirtualType SmallType a => Target (Virtual SmallType Imp.Variable a)
newRefV = lift $ mapVirtualA (const Imp.newVariable_) virtRep

initRefV :: VirtualType SmallType a => VExp a -> Target (Virtual SmallType Imp.Variable a)
initRefV = lift . mapVirtualA (Imp.newVariable)

getRefV :: VirtualType SmallType a => Virtual SmallType Imp.Variable a -> Target (VExp a)
getRefV = lift . mapVirtualA (Imp.getVariable)

setRefV :: VirtualType SmallType a => Virtual SmallType Imp.Variable a -> VExp a -> Target ()
setRefV r = lift . sequence_ . zipListVirtual (Imp.setVariable) r
{-
unsafeFreezeRefV :: VirtualType SmallType a =>
    Virtual SmallType Imp.Variable a -> Target (VExp a)
unsafeFreezeRefV = lift . mapVirtualA unsafeFreezeRef
-}
--------------------------------------------------------------------------------
-- * Translation of programs
--------------------------------------------------------------------------------

-- | Virtual expression
type VExp = Virtual SmallType Imp.VExp

-- | Virtual expression with hidden result type
data VExp'
  where
    VExp' :: Type a => Virtual SmallType Imp.VExp a -> VExp'

type TargetCMD =
        Imp.SignalCMD   Imp.VExp
  H.:+: Imp.VariableCMD Imp.VExp
  H.:+: Imp.ArrayCMD    Imp.VExp
  H.:+: Imp.LoopCMD     Imp.VExp

type Env = Map Name VExp'

-- | Target monad for translation
type Target = ReaderT Env (H.Program TargetCMD)

--------------------------------------------------------------------------------

pVType :: Proxy VType
pVType = Proxy

instance ShowClass VType where showClass _ = "VType"

deriveWitness ''VType ''BoolType
deriveWitness ''VType ''IntWordType

derivePWitness ''VType ''BoolType
derivePWitness ''VType ''IntWordType

instance PWitness VType CharType t
instance PWitness VType ListType t
instance PWitness VType TupleType t
instance PWitness VType FunType t

-- | Add a local alias to the environment
localAlias :: Type a
    => Name    -- ^ Old name
    -> VExp a  -- ^ New expression
    -> Target b
    -> Target b
localAlias v e = local (Map.insert v (VExp' e))

-- | Lookup an alias in the environment
lookAlias :: forall a . Type a => Name -> Target (VExp a)
lookAlias v = do
    env <- ask
    return $ case Map.lookup v env of
        Nothing | Right Dict <- pwit pVType tr
               -> error $ "lookAlias: variable " ++ show v ++ " not in scope"
        Just (VExp' e) -> case gcast pFeldTypes e of
            Left msg -> error $ "lookAlias: " ++ msg
            Right e' -> e'
  where
    tr = typeRep :: TypeRep FeldTypes a

-- | Translate instructions to the 'Target' monad
class Lower instr
  where
    lowerInstr :: instr Target a -> Target a

-- | Lift a 'VExp' that has been created using
-- 'Language.Embedded.Expression.litExp' or
-- 'Language.Embedded.Expression.varExp'
liftVar :: SmallType a => Imp.VExp a -> Data a
liftVar (Imp.VExp (Sym (Imp.T (dom))))
  | Just n@(Imp.Name _)  <- prj dom = Data $ Sym $ (inj (FreeVar (Imp.temporaryGetName n)) :&: typeRep)
  | Just (Imp.Literal l) <- prj dom = Feld.value l

{-
instance Lower (Imp.SignalCMD Data)
  where
    lowerInstr NewRef       = lift newRef
    lowerInstr (InitRef a)  = lift . initRef =<< translateSmallExp a
    lowerInstr (GetRef r)   = fmap liftVar $ lift $ getRef r
    lowerInstr (SetRef r a) = lift . setRef r =<< translateSmallExp a
    lowerInstr (UnsafeFreezeRef r) = fmap liftVar $ lift $ unsafeFreezeRef r
-}
{-
instance Lower (ArrCMD Data)
  where
    lowerInstr (NewArr n)     = lift . newArr =<< translateSmallExp n
    lowerInstr NewArr_        = lift newArr_
    lowerInstr (GetArr i arr) = do
        i' <- translateSmallExp i
        fmap liftVar $ lift $ getArr i' arr
    lowerInstr (SetArr i a arr) = do
        i' <- translateSmallExp i
        a' <- translateSmallExp a
        lift $ setArr i' a' arr
    lowerInstr (CopyArr dst src n) =
        lift . copyArr dst src =<< translateSmallExp n
-}
{-
instance Lower (ControlCMD Data)
  where
    lowerInstr (If c t f) = do
        c' <- translateSmallExp c
        ReaderT $ \env -> iff c'
            (flip runReaderT env t)
            (flip runReaderT env f)
    lowerInstr (While cont body) = do
        ReaderT $ \env -> while
            (flip runReaderT env $ translateSmallExp =<< cont)
            (flip runReaderT env body)
    lowerInstr (For (lo,step,hi) body) = do
        lo' <- translateSmallExp lo
        hi' <- traverse translateSmallExp hi
        ReaderT $ \env -> for (lo',step,hi') (flip runReaderT env . body . liftVar)
    lowerInstr (Assert cond msg) = do
        cond' <- translateSmallExp cond
        lift $ assert cond' msg
    lowerInstr Break = lift Imp.break

instance Lower (FileCMD Data)
  where
    lowerInstr (FOpen file mode)   = lift $ fopen file mode
    lowerInstr (FClose h)          = lift $ fclose h
    lowerInstr (FEof h)            = fmap liftVar $ lift $ feof h
    lowerInstr (FPrintf h form as) = lift . fprf h form . reverse =<< transPrintfArgs as
    lowerInstr (FGet h)            = fmap liftVar $ lift $ fget h

transPrintfArgs :: [PrintfArg Data] -> Target [PrintfArg CExp]
transPrintfArgs = mapM $ \(PrintfArg a) -> PrintfArg <$> translateSmallExp a

instance Lower (ObjectCMD Data)
  where
    lowerInstr (NewObject t) = lift $ newObject t
    lowerInstr (InitObject name True t as) = do
        lift . initObject name t =<< transFunArgs as
    lowerInstr (InitObject name False t as) = do
        lift . initUObject name t =<< transFunArgs as

transFunArgs :: [FunArg Data] -> Target [FunArg CExp]
transFunArgs = mapM $ mapMArg predCast translateSmallExp
  where
    predCast :: VarPredCast Data CExp
    predCast _ a = a

instance Lower (CallCMD Data)
  where
    lowerInstr (AddInclude incl)   = lift $ addInclude incl
    lowerInstr (AddDefinition def) = lift $ addDefinition def
    lowerInstr (AddExternFun f (_ :: proxy (Data res)) as) =
        lift . addExternFun f (Proxy :: Proxy (CExp res)) =<< transFunArgs as
    lowerInstr (AddExternProc p as) = lift . addExternProc p =<< transFunArgs as
    lowerInstr (CallFun f as)  = fmap liftVar . lift . callFun f =<< transFunArgs as
    lowerInstr (CallProc p as) = lift . callProc p =<< transFunArgs as

instance (Lower i1, Lower i2) => Lower (i1 Imp.:+: i2)
  where
    lowerInstr (Imp.Inl i) = lowerInstr i
    lowerInstr (Imp.Inr i) = lowerInstr i

-- | Translate a Feldspar program to the 'Target' monad
lower :: Program Feld.CMD a -> Target a
lower = interpretWithMonad lowerInstr

-- | Translate a Feldspar program a program that uses 'TargetCMD'
lowerTop :: Feld.Program a -> Program TargetCMD a
lowerTop = flip runReaderT Map.empty . lower . unProgram
-}
--------------------------------------------------------------------------------
-- * Translation of expressions
--------------------------------------------------------------------------------

transAST :: ASTF FeldDomain a -> Target (VExp a)
transAST = goAST . optimize
  where
    goAST :: ASTF FeldDomain a -> Target (VExp a)
    goAST = simpleMatch (\(s :&: t) -> go t s)

    goSmallAST :: SmallType a => ASTF FeldDomain a -> Target (Imp.VExp a)
    goSmallAST = fmap viewActual . goAST

    go :: TypeRep FeldTypes (DenResult sig)
       -> FeldConstructs sig
       -> Args (AST FeldDomain) sig
       -> Target (VExp (DenResult sig))
    go = undefined
{-
    go t lit Nil
        | Just (Literal a) <- prj lit
        , Right Dict <- pwit pType t
        = return $ mapVirtual (value . runIdentity) $ toVirtual a
    go t var Nil
        | Just (VarT v) <- prj var
        , Right Dict <- pwit pType t
        = lookAlias v
    go t lt (a :* (lam :$ body) :* Nil)
        | Just Let      <- prj lt
        , Just (LamT v) <- prj lam
        , Right Dict    <- pwit pType (getDecor a)
        = do r  <- initRefV =<< goAST a
             a' <- unsafeFreezeRefV r
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
        | Just I2N <- prj op = liftVirt i2n  <$> goAST a
        | Just Not <- prj op = liftVirt not_ <$> goAST a
    go t op (a :* b :* Nil)
        | Just Add <- prj op = liftVirt2 (+)   <$> goAST a <*> goAST b
        | Just Sub <- prj op = liftVirt2 (-)   <$> goAST a <*> goAST b
        | Just Mul <- prj op = liftVirt2 (*)   <$> goAST a <*> goAST b
        | Just Eq  <- prj op = liftVirt2 (#==) <$> goAST a <*> goAST b
        | Just Lt  <- prj op = liftVirt2 (#<)  <$> goAST a <*> goAST b
        | Just Gt  <- prj op = liftVirt2 (#>)  <$> goAST a <*> goAST b
        | Just Le  <- prj op = liftVirt2 (#<=) <$> goAST a <*> goAST b
        | Just Ge  <- prj op = liftVirt2 (#>=) <$> goAST a <*> goAST b
    go ty cond (c :* t :* f :* Nil)
        | Just Condition <- prj cond = do
            env <- ask
            case () of
              _ | Imp.Return (Actual t') <- Imp.view $ flip runReaderT env $ goAST t
                , Imp.Return (Actual f') <- Imp.view $ flip runReaderT env $ goAST f
                -> do
                    c' <- goSmallAST c
                    return $ Actual (c' ? t' $ f')
              _ -> do
                    c'  <- goSmallAST c
                    res <- newRefV
                    ReaderT $ \env -> iff c'
                        (flip runReaderT env $ goAST t >>= setRefV res)
                        (flip runReaderT env $ goAST f >>= setRefV res)
                    unsafeFreezeRefV res
    go t loop (len :* init :* (lami :$ (lams :$ body)) :* Nil)
        | Just ForLoop   <- prj loop
        , Just (LamT iv) <- prj lami
        , Just (LamT sv) <- prj lams
        = do len'  <- goSmallAST len
             state <- initRefV =<< goAST init
             ReaderT $ \env -> for (0, 1, Excl len') $ \i -> flip runReaderT env $ do
                s <- case pwit pSmallType t of
                    Right Dict -> unsafeFreezeRefV state  -- For non-compound states
                    _          -> getRefV state
                s' <- localAlias iv (Actual i) $
                        localAlias sv s $
                          goAST body
                setRefV state s'
             unsafeFreezeRefV state
    go t free Nil
        | Just (FreeVar v) <- prj free = return $ Actual $ Imp.variable v
    go t arrIx (i :* Nil)
        | Just (UnsafeArrIx arr) <- prj arrIx = do
            i' <- goSmallAST i
            fmap Actual $ lift $ Feld.getArr i' arr
    go t unsPerf Nil
        | Just (UnsafePerform prog) <- prj unsPerf
        = translateExp =<< lower (unProgram prog)
    go t unsPerf (a :* Nil)
        | Just (UnsafePerformWith prog) <- prj unsPerf = do
            a' <- goAST a
            lower (unProgram prog)
            return a'
-}
-- | Translate a Feldspar expression
translateExp :: Data a -> Target (VExp a)
translateExp = transAST . unData

-- | Translate a Feldspar expression that can be represented as a simple 'VExp'
translateSmallExp :: SmallType a => Data a -> Target (Imp.VExp a)
translateSmallExp = fmap viewActual . translateExp

--------------------------------------------------------------------------------
-- * Back ends
--------------------------------------------------------------------------------
{-
-- | Interpret a program in the 'IO' monad
runIO :: Feld.Program a -> IO a
runIO = Imp.interpret . lowerTop

-- | Compile a program to C code represented as a string. To compile the
-- resulting C code, use something like
--
-- > gcc -std=c99 YOURPROGRAM.c
compile :: Feld.Program a -> String
compile = Imp.compile . lowerTop

-- | Compile a program to C code and print it on the screen. To compile the
-- resulting C code, use something like
--
-- > gcc -std=c99 YOURPROGRAM.c
icompile :: Feld.Program a -> IO ()
icompile = putStrLn . compile

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck' :: Feld.ExternalCompilerOpts -> Feld.Program a -> IO ()
compileAndCheck' opts = Imp.compileAndCheck' opts . lowerTop

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck :: Feld.Program a -> IO ()
compileAndCheck = compileAndCheck' mempty

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled' :: Feld.ExternalCompilerOpts -> Feld.Program a -> IO ()
runCompiled' opts = Imp.runCompiled' opts . lowerTop

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled :: Feld.Program a -> IO ()
runCompiled = runCompiled' mempty

-- | Like 'runCompiled'' but with explicit input/output connected to
-- @stdin@/@stdout@
captureCompiled'
    :: Feld.ExternalCompilerOpts
    -> Feld.Program a  -- ^ Program to run
    -> String          -- ^ Input to send to @stdin@
    -> IO String       -- ^ Result from @stdout@
captureCompiled' opts = Imp.captureCompiled' opts . lowerTop

-- | Like 'runCompiled' but with explicit input/output connected to
-- @stdin@/@stdout@
captureCompiled
    :: Feld.Program a  -- ^ Program to run
    -> String          -- ^ Input to send to @stdin@
    -> IO String       -- ^ Result from @stdout@
captureCompiled = captureCompiled' mempty

-- | Compare the content written to 'stdout' from interpretation in 'IO' and
-- from running the compiled C code
compareCompiled'
    :: Feld.ExternalCompilerOpts
    -> Feld.Program a  -- ^ Program to run
    -> String          -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled' opts = Imp.compareCompiled' opts . lowerTop

-- | Compare the content written to 'stdout' from interpretation in 'IO' and
-- from running the compiled C code
compareCompiled
    :: Feld.Program a  -- ^ Program to run
    -> String          -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled = compareCompiled' mempty
-}
--------------------------------------------------------------------------------
