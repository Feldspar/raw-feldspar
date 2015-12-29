{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Compile where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Monoid
import Data.Traversable (traverse)
#endif

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Control.Monad.Operational.Higher as Imp
import Language.Syntactic hiding ((:+:) (..), (:<:) (..))
import Language.Syntactic.Functional hiding (Binding (..))
import Language.Syntactic.Functional.Tuple

import Data.TypeRep

import Control.Monad.Operational.Higher (interpretWithMonad)

import Language.Embedded.Imperative hiding ((:+:) (..), (:<:) (..))
import qualified Language.Embedded.Imperative as Imp
import Language.Embedded.Imperative.CMD hiding (Ref, Arr)
import Language.Embedded.CExp
import qualified Language.Embedded.Backend.C as Imp

import Data.VirtualContainer
import Feldspar.Representation hiding (Program)
import qualified Feldspar.Representation as Feld
import qualified Feldspar.Frontend as Feld



--------------------------------------------------------------------------------
-- * Virtual variables
--------------------------------------------------------------------------------

newRefV :: VirtualType SmallType a => Target (Virtual SmallType Imp.Ref a)
newRefV = lift $ mapVirtualA (const newRef) virtRep

initRefV :: VirtualType SmallType a =>
    VExp a -> Target (Virtual SmallType Imp.Ref a)
initRefV = lift . mapVirtualA initRef

getRefV :: VirtualType SmallType a =>
    Virtual SmallType Imp.Ref a -> Target (VExp a)
getRefV = lift . mapVirtualA getRef

setRefV :: VirtualType SmallType a =>
    Virtual SmallType Imp.Ref a -> VExp a -> Target ()
setRefV r = lift . sequence_ . zipListVirtual setRef r

unsafeFreezeRefV :: VirtualType SmallType a =>
    Virtual SmallType Imp.Ref a -> Target (VExp a)
unsafeFreezeRefV = lift . mapVirtualA unsafeFreezeRef



--------------------------------------------------------------------------------
-- * Translation of programs
--------------------------------------------------------------------------------

-- | Virtual expression
type VExp = Virtual SmallType CExp

-- | Virtual expression with hidden result type
data VExp'
  where
    VExp' :: Type a => Virtual SmallType CExp a -> VExp'

type TargetCMD
    =       RefCMD CExp
    Imp.:+: ArrCMD CExp
    Imp.:+: ControlCMD CExp
    Imp.:+: FileCMD CExp
    Imp.:+: ObjectCMD CExp
    Imp.:+: CallCMD CExp

type Env = Map Name VExp'

-- | Target monad for translation
type Target = ReaderT Env (Program TargetCMD)

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
        Nothing | Right Dict <- pwit pCType tr
               -> error $  "lookAlias: variable " ++ show v ++ " not in scope"
        Just (VExp' e) -> case gcast pFeldTypes e of
            Left msg -> error $ "lookAlias: " ++ msg
            Right e' -> e'
  where
    tr = typeRep :: TypeRep FeldTypes a

-- | Translate instructions to the 'Target' monad
class Lower instr
  where
    lowerInstr :: instr Target a -> Target a

-- | Lift a 'CExp' that has been created using
-- 'Language.Embedded.Expression.litExp' or
-- 'Language.Embedded.Expression.varExp'
liftVar :: SmallType a => CExp a -> Data a
liftVar (CExp (Sym (T (Var v))))   = Data $ Sym $ (inj (FreeVar v) :&: typeRep)
liftVar (CExp (Sym (T (Fun _ a)))) = Feld.value a

instance Lower (RefCMD Data)
  where
    lowerInstr NewRef      = lift newRef
    lowerInstr (InitRef a) = do
        Actual a' <- translateExp a
        lift $ initRef a'
    lowerInstr (GetRef r)   = fmap liftVar $ lift $ getRef r
    lowerInstr (SetRef r a) = do
        Actual a' <- translateExp a
        lift $ setRef r a'
    lowerInstr (UnsafeFreezeRef r) = fmap liftVar $ lift $ unsafeFreezeRef r

instance Lower (ArrCMD Data)
  where
    lowerInstr (NewArr n) = do
        Actual n' <- translateExp n
        lift $ newArr n'
    lowerInstr NewArr_ = lift newArr_
    lowerInstr (GetArr i arr) = do
        Actual i' <- translateExp i
        fmap liftVar $ lift $ getArr i' arr
    lowerInstr (SetArr i a arr) = do
        Actual i' <- translateExp i
        Actual a' <- translateExp a
        lift $ setArr i' a' arr

instance Lower (ControlCMD Data)
  where
    lowerInstr (If c t f) = do
        Actual c' <- translateExp c
        ReaderT $ \env -> iff c'
            (flip runReaderT env t)
            (flip runReaderT env f)
    lowerInstr (While cont body) = do
        ReaderT $ \env -> while
            (flip runReaderT env $ do
                Actual c' <- translateExp =<< cont
                return c'
            )
            (flip runReaderT env body)
    lowerInstr (For (lo,step,hi) body) = do
        Actual lo' <- translateExp lo
        hi' <- traverse (fmap (\(Actual c) -> c) . translateExp) hi
        ReaderT $ \env -> for (lo',step,hi') (flip runReaderT env . body . liftVar)
    lowerInstr Break = lift Imp.break

instance Lower (FileCMD Data)
  where
    lowerInstr (FOpen file mode)   = lift $ fopen file mode
    lowerInstr (FClose h)          = lift $ fclose h
    lowerInstr (FEof h)            = fmap liftVar $ lift $ feof h
    lowerInstr (FPrintf h form as) = lift . fprf h form . reverse =<< transPrintfArgs as
    lowerInstr (FGet h)            = fmap liftVar $ lift $ fget h

transPrintfArgs :: [PrintfArg Data] -> Target [PrintfArg CExp]
transPrintfArgs = mapM $ \(PrintfArg a) -> do
    Actual a' <- translateExp a
    return $ PrintfArg a'

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



--------------------------------------------------------------------------------
-- * Translation of expressions
--------------------------------------------------------------------------------

transAST :: forall a . ASTF FeldDomain a -> Target (VExp a)
transAST a = simpleMatch (\(s :&: t) -> go t s) a
  where
    go :: TypeRep FeldTypes (DenResult sig) -> FeldConstructs sig
       -> Args (AST FeldDomain) sig -> Target (VExp (DenResult sig))
    go t lit Nil
        | Just (Literal a) <- prj lit
        , Right Dict <- pwit pSmallType t
        = return $ Actual $ value a
    go t var Nil
        | Just (VarT v) <- prj var
        , Right Dict <- pwit pType t
        = lookAlias v
    go t lt (a :* (lam :$ body) :* Nil)
        | Just Let      <- prj lt
        , Just (LamT v) <- prj lam
        , Right Dict    <- pwit pType (getDecor a)
        = do r  <- initRefV =<< transAST a
             a' <- unsafeFreezeRefV r
             localAlias v a' $ transAST body
    go t tup (a :* b :* Nil)
        | Just Tup2 <- prj tup = VTup2 <$> transAST a <*> transAST b
    go t tup (a :* b :* c :* Nil)
        | Just Tup3 <- prj tup = VTup3 <$> transAST a <*> transAST b <*> transAST c
    go t tup (a :* b :* c :* d :* Nil)
        | Just Tup4 <- prj tup = VTup4 <$> transAST a <*> transAST b <*> transAST c <*> transAST d
    go t sel (a :* Nil)
        | Just Sel1  <- prj sel = fmap vsel1  $ transAST a
        | Just Sel2  <- prj sel = fmap vsel2  $ transAST a
        | Just Sel3  <- prj sel = fmap vsel3  $ transAST a
        | Just Sel4  <- prj sel = fmap vsel4  $ transAST a
        | Just Sel5  <- prj sel = fmap vsel5  $ transAST a
        | Just Sel6  <- prj sel = fmap vsel6  $ transAST a
        | Just Sel7  <- prj sel = fmap vsel7  $ transAST a
        | Just Sel8  <- prj sel = fmap vsel8  $ transAST a
        | Just Sel9  <- prj sel = fmap vsel9  $ transAST a
        | Just Sel10 <- prj sel = fmap vsel10 $ transAST a
        | Just Sel11 <- prj sel = fmap vsel11 $ transAST a
        | Just Sel12 <- prj sel = fmap vsel12 $ transAST a
        | Just Sel13 <- prj sel = fmap vsel13 $ transAST a
        | Just Sel14 <- prj sel = fmap vsel14 $ transAST a
        | Just Sel15 <- prj sel = fmap vsel15 $ transAST a
    go t op (a :* Nil)
        | Just I2N <- prj op = liftVirt i2n  <$> transAST a
        | Just Not <- prj op = liftVirt not_ <$> transAST a
    go t op (a :* b :* Nil)
        | Just Add <- prj op = liftVirt2 (+)   <$> transAST a <*> transAST b
        | Just Sub <- prj op = liftVirt2 (-)   <$> transAST a <*> transAST b
        | Just Mul <- prj op = liftVirt2 (*)   <$> transAST a <*> transAST b
        | Just Eq  <- prj op = liftVirt2 (#==) <$> transAST a <*> transAST b
        | Just Lt  <- prj op = liftVirt2 (#<)  <$> transAST a <*> transAST b
        | Just Gt  <- prj op = liftVirt2 (#>)  <$> transAST a <*> transAST b
        | Just Le  <- prj op = liftVirt2 (#<=) <$> transAST a <*> transAST b
        | Just Ge  <- prj op = liftVirt2 (#>=) <$> transAST a <*> transAST b
    go ty cond (c :* t :* f :* Nil)
        | Just Condition <- prj cond = do
            env <- ask
            case () of
              _ | Imp.Return (Actual t') <- Imp.view $ flip runReaderT env $ transAST t
                , Imp.Return (Actual f') <- Imp.view $ flip runReaderT env $ transAST f
                -> do
                    Actual c' <- transAST c
                    return $ Actual (c' ? t' $ f')
              _ -> do
                    Actual c' <- transAST c
                    res <- newRefV
                    ReaderT $ \env -> iff c'
                        (flip runReaderT env $ transAST t >>= setRefV res)
                        (flip runReaderT env $ transAST f >>= setRefV res)
                    unsafeFreezeRefV res
    go t loop (len :* init :* (lami :$ (lams :$ body)) :* Nil)
        | Just ForLoop   <- prj loop
        , Just (LamT iv) <- prj lami
        , Just (LamT sv) <- prj lams
        = do Actual len' <- transAST len
             state <- initRefV =<< transAST init
             ReaderT $ \env -> for (0, 1, Excl len') $ \i -> flip runReaderT env $ do
                s <- case pwit pSmallType t of
                    Right Dict -> unsafeFreezeRefV state  -- For non-compound states
                    _          -> getRefV state
                s' <- localAlias iv (Actual i) $
                        localAlias sv s $
                          transAST body
                setRefV state s'
             unsafeFreezeRefV state
    go t free Nil
        | Just (FreeVar v) <- prj free = return $ Actual $ variable v
    go t arrIx (i :* Nil)
        | Just (UnsafeArrIx arr) <- prj arrIx = do
            Actual i' <- transAST i
            fmap Actual $ lift $ getArr i' arr
    go t unsPerf Nil
        | Just (UnsafePerform prog) <- prj unsPerf
        = translateExp =<< lower (unProgram prog)
    go t unsPerf (a :* Nil)
        | Just (UnsafePerformWith prog) <- prj unsPerf = do
            a' <- transAST a
            lower (unProgram prog)
            return a'

-- | Translate a Feldspar expression
translateExp :: Data a -> Target (VExp a)
translateExp = transAST . unData

-- | Translate a Feldspar expression that can be represented as a simple 'CExp'
translateSmallExp :: SmallType a => Data a -> Target (CExp a)
translateSmallExp a = do
    Actual a <- translateExp a
    return a



--------------------------------------------------------------------------------
-- * Back ends
--------------------------------------------------------------------------------

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

