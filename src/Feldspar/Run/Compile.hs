{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Run.Compile where



import Control.Monad.Identity
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

import Language.Syntactic hiding ((:+:) (..), (:<:) (..))
import Language.Syntactic.Functional hiding (Binding (..))
import Language.Syntactic.Functional.Tuple

import Data.TypeRep

import qualified Control.Monad.Operational.Higher as Oper

import Language.Embedded.Expression
import Language.Embedded.Imperative hiding ((:+:) (..), (:<:) (..))
import Language.Embedded.Concurrent
import qualified Language.Embedded.Imperative as Imp
import Language.Embedded.CExp
import Language.Embedded.Backend.C (ExternalCompilerOpts (..))
import qualified Language.Embedded.Backend.C as Imp

import Data.VirtualContainer
import Feldspar.Representation
import Feldspar.Run.Representation
import Feldspar.Optimize



--------------------------------------------------------------------------------
-- * Virtual expressions and variables
--------------------------------------------------------------------------------

-- | Virtual expression
type VExp = Virtual SmallType CExp

-- | Virtual expression with hidden result type
data VExp'
  where
    VExp' :: Type a => Virtual SmallType CExp a -> VExp'

newRefV :: (VirtualType SmallType a, Monad m) =>
    String -> TargetT m (Virtual SmallType Imp.Ref a)
newRefV base = lift $ mapVirtualA (const (newNamedRef base)) virtRep

initRefV :: (VirtualType SmallType a, Monad m) =>
    String -> VExp a -> TargetT m (Virtual SmallType Imp.Ref a)
initRefV base = lift . mapVirtualA (initNamedRef base)

getRefV :: (VirtualType SmallType a, Monad m) =>
    Virtual SmallType Imp.Ref a -> TargetT m (VExp a)
getRefV = lift . mapVirtualA getRef

setRefV :: (VirtualType SmallType a, Monad m) =>
    Virtual SmallType Imp.Ref a -> VExp a -> TargetT m ()
setRefV r = lift . sequence_ . zipListVirtual setRef r

unsafeFreezeRefV :: (VirtualType SmallType a, Monad m) =>
    Virtual SmallType Imp.Ref a -> TargetT m (VExp a)
unsafeFreezeRefV = lift . mapVirtualA unsafeFreezeRef



--------------------------------------------------------------------------------
-- * Translation environment
--------------------------------------------------------------------------------

-- | Translation environment
type Env = Map Name VExp'

-- | Add a local alias to the environment
localAlias :: (Type a, MonadReader Env m)
    => Name    -- ^ Old name
    -> VExp a  -- ^ New expression
    -> m b
    -> m b
localAlias v e = local (Map.insert v (VExp' e))

-- | Lookup an alias in the environment
lookAlias :: (Type a, MonadReader Env m) => Name -> m (VExp a)
lookAlias v = do
    env <- ask
    return $ case Map.lookup v env of
        Nothing -> error $ "lookAlias: variable " ++ show v ++ " not in scope"
        Just (VExp' e) -> case gcast pFeldTypes e of
            Left msg -> error $ "lookAlias: " ++ msg
            Right e' -> e'



--------------------------------------------------------------------------------
-- * Translation of expressions
--------------------------------------------------------------------------------

type TargetCMD
    =       RefCMD
    Imp.:+: ArrCMD
    Imp.:+: ControlCMD
    Imp.:+: ThreadCMD
    Imp.:+: ChanCMD
    Imp.:+: PtrCMD
    Imp.:+: FileCMD
    Imp.:+: C_CMD

-- | Target monad during translation
type TargetT m = ReaderT Env (ProgramT TargetCMD (Param2 CExp CType) m)

-- | Monad for translated program
type ProgC = Program TargetCMD (Param2 CExp CType)

-- | Translate an expression
translateExp :: forall m a . Monad m => Data a -> TargetT m (VExp a)
translateExp = goAST . optimize . unData
  where
    goAST :: ASTF FeldDomain b -> TargetT m (VExp b)
    goAST = simpleMatch (\(s :&: t) -> go t s)

    goSmallAST :: SmallType b => ASTF FeldDomain b -> TargetT m (CExp b)
    goSmallAST = fmap viewActual . goAST

    go :: TypeRep FeldTypes (DenResult sig)
       -> FeldConstructs sig
       -> Args (AST FeldDomain) sig
       -> TargetT m (VExp (DenResult sig))
    go t lit Nil
        | Just (Literal a) <- prj lit
        , Right Dict <- pwit pType t
        = return $ mapVirtual (value . runIdentity) $ toVirtual a
    go t var Nil
        | Just (VarT v) <- prj var
        , Right Dict <- pwit pType t
        = lookAlias v
    go t lt (a :* (lam :$ body) :* Nil)
        | Just (Let tag) <- prj lt
        , Just (LamT v)  <- prj lam
        , Right Dict     <- pwit pType (getDecor a)
        = do let base = if null tag then "let" else tag
             r  <- initRefV base =<< goAST a
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
    go t c Nil
        | Just Pi <- prj c = return $ Actual pi
    go t op (a :* Nil)
        | Just Neg   <- prj op = liftVirt negate <$> goAST a
        | Just Sin   <- prj op = liftVirt sin    <$> goAST a
        | Just Cos   <- prj op = liftVirt cos    <$> goAST a
        | Just I2N   <- prj op = liftVirt i2n    <$> goAST a
        | Just I2B   <- prj op = liftVirt i2b    <$> goAST a
        | Just B2I   <- prj op = liftVirt b2i    <$> goAST a
        | Just Round <- prj op = liftVirt round_ <$> goAST a
        | Just Not   <- prj op = liftVirt not_   <$> goAST a
    go t op (a :* b :* Nil)
        | Just Add  <- prj op = liftVirt2 (+)   <$> goAST a <*> goAST b
        | Just Sub  <- prj op = liftVirt2 (-)   <$> goAST a <*> goAST b
        | Just Mul  <- prj op = liftVirt2 (*)   <$> goAST a <*> goAST b
        | Just FDiv <- prj op = liftVirt2 (/)   <$> goAST a <*> goAST b
        | Just Quot <- prj op = liftVirt2 quot_ <$> goAST a <*> goAST b
        | Just Rem  <- prj op = liftVirt2 (#%)  <$> goAST a <*> goAST b
        | Just Pow  <- prj op = liftVirt2 (**)  <$> goAST a <*> goAST b
        | Just Eq   <- prj op = liftVirt2 (#==) <$> goAST a <*> goAST b
        | Just And  <- prj op = liftVirt2 (#&&) <$> goAST a <*> goAST b
        | Just Or   <- prj op = liftVirt2 (#||) <$> goAST a <*> goAST b
        | Just Lt   <- prj op = liftVirt2 (#<)  <$> goAST a <*> goAST b
        | Just Gt   <- prj op = liftVirt2 (#>)  <$> goAST a <*> goAST b
        | Just Le   <- prj op = liftVirt2 (#<=) <$> goAST a <*> goAST b
        | Just Ge   <- prj op = liftVirt2 (#>=) <$> goAST a <*> goAST b
    go t arrIx (i :* Nil)
        | Just (Feldspar.Representation.ArrIx arr) <- prj arrIx = do
            i' <- goSmallAST i
            return $ Actual (arr #! i')
    go ty cond (c :* t :* f :* Nil)
        | Just Condition <- prj cond = do
            env <- ask
            case (flip runReaderT env $ goAST t, flip runReaderT env $ goAST f) of
              (t',f') -> do
                  tView <- lift $ lift $ Oper.viewT t'
                  fView <- lift $ lift $ Oper.viewT f'
                  case (tView,fView) of
                      (Oper.Return (Actual tExp), Oper.Return (Actual fExp)) -> do
                          c' <- goSmallAST c
                          return $ Actual (c' ? tExp $ fExp)
                      _ -> do
                          c'  <- goSmallAST c
                          res <- newRefV "v"
                          ReaderT $ \env -> iff c'
                              (flip runReaderT env . setRefV res =<< t')
                              (flip runReaderT env . setRefV res =<< f')
                          unsafeFreezeRefV res
    go t loop (len :* init :* (lami :$ (lams :$ body)) :* Nil)
        | Just ForLoop   <- prj loop
        , Just (LamT iv) <- prj lami
        , Just (LamT sv) <- prj lams
        = do len'  <- goSmallAST len
             state <- initRefV "state" =<< goAST init
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
        | Just (FreeVar v) <- prj free = return $ Actual $ variable v
    go t unsPerf Nil
        | Just (UnsafePerform prog) <- prj unsPerf
        = translateExp =<< Oper.reexpressEnv unsafeTransSmallExp (Oper.liftProgram $ unComp prog)
    go t unsPerf (a :* Nil)
        | Just (UnsafePerformWith prog) <- prj unsPerf = do
            a' <- goAST a
            Oper.reexpressEnv unsafeTransSmallExp (Oper.liftProgram $ unComp prog)
            return a'

-- | Translate an expression that is assumed to fulfill @`SmallType` a@
unsafeTransSmallExp :: Monad m => Data a -> TargetT m (CExp a)
unsafeTransSmallExp a = do
    Actual b <- translateExp a
    return b
  -- This function should ideally have a `SmallType a` constraint, but that
  -- is not allowed when passing it to `reexpressEnv`. It should be possible
  -- to make it work by changing the interface to `reexpressEnv`.

translate :: Run a -> ProgC a
translate
    = Oper.interpretWithMonadT Oper.singleton id
        -- fuse the monad stack
    . flip runReaderT Map.empty . Oper.reexpressEnv unsafeTransSmallExp
        -- compile outer monad
    . Oper.interpretWithMonadT Oper.singleton
        (lift . flip runReaderT Map.empty . Oper.reexpressEnv unsafeTransSmallExp)
        -- compile inner monad
    . unRun



--------------------------------------------------------------------------------
-- * Back ends
--------------------------------------------------------------------------------

-- | Interpret a program in the 'IO' monad
runIO :: MonadRun m => m a -> IO a
runIO = Imp.runIO . translate . liftRun

-- | Interpret a program in the 'IO' monad
runIO' :: MonadRun m => m a -> IO a
runIO'
    = Oper.interpretWithMonadBiT
        (return . evalExp)
        Oper.interpBi
        (Imp.interpretBi (return . evalExp))
    . unRun
    . liftRun
  -- Unlike `runIO`, this function does the interpretation directly, without
  -- first lowering the program. This might be faster, but I haven't done any
  -- measurements to se if it is.
  --
  -- One disadvantage with `runIO'` is that it cannot handle expressions
  -- involving `IOSym`. But at the moment of writing this, we're not using those
  -- symbols for anything anyway.

-- | Like 'runIO' but with explicit input/output connected to @stdin@/@stdout@
captureIO :: MonadRun m
    => m a        -- ^ Program to run
    -> String     -- ^ Input to send to @stdin@
    -> IO String  -- ^ Result from @stdout@
captureIO = Imp.captureIO . translate . liftRun

-- | Compile a program to C code represented as a string. To compile the
-- resulting C code, use something like
--
-- > gcc -std=c99 YOURPROGRAM.c
--
-- This function returns only the first (main) module. To get all C translation
-- unit, use 'compileAll'.
compile :: MonadRun m => m a -> String
compile = Imp.compile . translate . liftRun

-- | Compile a program to C modules, each one represented as a pair of a name
-- and the code represented as a string
--
-- To compile the resulting C code, use something like
--
-- > gcc -std=c99 YOURPROGRAM.c
compileAll :: MonadRun m => m a -> [(String, String)]
compileAll = Imp.compileAll . translate . liftRun

-- | Compile a program to C code and print it on the screen. To compile the
-- resulting C code, use something like
--
-- > gcc -std=c99 YOURPROGRAM.c
icompile :: MonadRun m => m a -> IO ()
icompile = Imp.icompile . translate . liftRun

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck' :: MonadRun m => ExternalCompilerOpts -> m a -> IO ()
compileAndCheck' opts = Imp.compileAndCheck' opts . translate . liftRun

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck :: MonadRun m => m a -> IO ()
compileAndCheck = compileAndCheck' mempty

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled' :: MonadRun m => ExternalCompilerOpts -> m a -> IO ()
runCompiled' opts = Imp.runCompiled' opts . translate . liftRun

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled :: MonadRun m => m a -> IO ()
runCompiled = runCompiled' mempty

-- | Like 'runCompiled'' but with explicit input/output connected to
-- @stdin@/@stdout@
captureCompiled' :: MonadRun m
    => ExternalCompilerOpts
    -> m a        -- ^ Program to run
    -> String     -- ^ Input to send to @stdin@
    -> IO String  -- ^ Result from @stdout@
captureCompiled' opts = Imp.captureCompiled' opts . translate . liftRun

-- | Like 'runCompiled' but with explicit input/output connected to
-- @stdin@/@stdout@
captureCompiled :: MonadRun m
    => m a        -- ^ Program to run
    -> String     -- ^ Input to send to @stdin@
    -> IO String  -- ^ Result from @stdout@
captureCompiled = captureCompiled' mempty

-- | Compare the content written to @stdout@ from the reference program and from
-- running the compiled C code
compareCompiled' :: MonadRun m
    => ExternalCompilerOpts
    -> m a     -- ^ Program to run
    -> IO a    -- ^ Reference program
    -> String  -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled' opts = Imp.compareCompiled' opts . translate . liftRun

-- | Compare the content written to @stdout@ from the reference program and from
-- running the compiled C code
compareCompiled :: MonadRun m
    => m a     -- ^ Program to run
    -> IO a    -- ^ Reference program
    -> String  -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled = compareCompiled' mempty

