{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Run.Compile where



import Control.Monad.Identity
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (gcast)

import Data.Constraint (Dict (..))

import Language.Syntactic hiding ((:+:) (..), (:<:) (..))
import Language.Syntactic.Functional hiding (Binding (..))
import Language.Syntactic.Functional.Tuple

import qualified Control.Monad.Operational.Higher as Oper

import Language.Embedded.Expression
import Language.Embedded.Imperative hiding ((:+:) (..), (:<:) (..))
import Language.Embedded.Concurrent
import qualified Language.Embedded.Imperative as Imp
import Language.Embedded.Backend.C (ExternalCompilerOpts (..))
import qualified Language.Embedded.Backend.C as Imp

import Data.TypedStruct
import Feldspar.Primitive.Representation
import Feldspar.Primitive.Backend.C ()
import Feldspar.Representation
import Feldspar.Run.Representation
import Feldspar.Optimize



--------------------------------------------------------------------------------
-- * Struct expressions and variables
--------------------------------------------------------------------------------

-- | Struct expression
type VExp = Struct PrimType Prim

-- | Struct expression with hidden result type
data VExp'
  where
    VExp' :: Type a => Struct PrimType Prim a -> VExp'

newRefV :: (Type a, Monad m) => String -> TargetT m (Struct PrimType Imp.Ref a)
newRefV base = error "TODO"  -- lift $ mapStructA (const (newNamedRef base)) typeRep

initRefV :: Monad m => String -> VExp a -> TargetT m (Struct PrimType Imp.Ref a)
initRefV base = lift . mapStructA (initNamedRef base)

getRefV :: (Type a, Monad m) => Struct PrimType Imp.Ref a -> TargetT m (VExp a)
getRefV = lift . mapStructA getRef

setRefV :: Monad m => Struct PrimType Imp.Ref a -> VExp a -> TargetT m ()
setRefV r = lift . sequence_ . zipListStruct setRef r

unsafeFreezeRefV :: Monad m => Struct PrimType Imp.Ref a -> TargetT m (VExp a)
unsafeFreezeRefV = lift . mapStructA unsafeFreezeRef



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
        Just (VExp' e) -> case gcast e of Just e' -> e'



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
type TargetT m = ReaderT Env (ProgramT TargetCMD (Param2 Prim PrimType') m)

-- | Monad for translated program
type ProgC = Program TargetCMD (Param2 Prim PrimType')

-- | Translate an expression
translateExp :: forall m a . Monad m => Data a -> TargetT m (VExp a)
translateExp = goAST . optimize . unData
  where
    goAST :: ASTF FeldDomain b -> TargetT m (VExp b)
    goAST = simpleMatch (\(s :&: ValT t) -> go t s)
      -- TODO comment on the match

    goSmallAST :: PrimType b => ASTF FeldDomain b -> TargetT m (Prim b)
    goSmallAST = fmap extractSingle . goAST

    go :: TypeRep (DenResult sig)
       -> FeldConstructs sig
       -> Args (AST FeldDomain) sig
       -> TargetT m (VExp (DenResult sig))
    go t lit Nil
        | Just (Literal a) <- prj lit
        , Single _ <- t  -- TODO comment on the match
        = return $ mapStruct (constExp . runIdentity) $ toStruct a
    go t var Nil
        | Just (VarT v) <- prj var
        , Single _ <- t  -- TODO comment on the match
        = lookAlias v
    go t lt (a :* (lam :$ body) :* Nil)
        | Just (Let tag) <- prj lt
        , Just (LamT v)  <- prj lam
        , Single _ <- t  -- TODO comment on the match
        , Just Dict <- witTypeFun $ getDecor a  -- TODO comment on the match
        = do let base = if null tag then "let" else tag
             r  <- initRefV base =<< goAST a
             a' <- unsafeFreezeRefV r
             localAlias v a' $ goAST body
    go t tup (a :* b :* Nil)
        | Just Pair <- prj tup = Two <$> goAST a <*> goAST b
    go t sel (ab :* Nil)
        | Just Fst <- prj sel = do
            Two a _ <- goAST ab
            return a
        | Just Snd <- prj sel = do
            Two _ b <- goAST ab
            return b
    go (Single _) c Nil  -- TODO comment on the match
        | Just Pi <- prj c = return $ Single $ Prim $ Sym (Pi :&: primTypeRep)
    go (Single _) op (a :* Nil)  -- TODO comment on the match
        | Just Neg   <- prj op = liftStruct negate <$> goAST a
--         | Just Sin   <- prj op = liftStruct sin    <$> goAST a
--         | Just Cos   <- prj op = liftStruct cos    <$> goAST a
--         | Just I2N   <- prj op = liftStruct iii    <$> goAST a
--         | Just I2B   <- prj op = liftStruct i2b    <$> goAST a
--         | Just B2I   <- prj op = liftStruct b2i    <$> goAST a
--         | Just Round <- prj op = liftStruct round_ <$> goAST a
--         | Just Not   <- prj op = liftStruct not_   <$> goAST a
    go (Single _) op (a :* b :* Nil)  -- TODO comment on the match
        | Just Add  <- prj op = liftStruct2 (+)   <$> goAST a <*> goAST b
        | Just Sub  <- prj op = liftStruct2 (-)   <$> goAST a <*> goAST b
        | Just Mul  <- prj op = liftStruct2 (*)   <$> goAST a <*> goAST b
--         | Just FDiv <- prj op = liftStruct2 (/)   <$> goAST a <*> goAST b
--         | Just Quot <- prj op = liftStruct2 quot_ <$> goAST a <*> goAST b
--         | Just Rem  <- prj op = liftStruct2 (#%)  <$> goAST a <*> goAST b
--         | Just Pow  <- prj op = liftStruct2 (**)  <$> goAST a <*> goAST b
--         | Just Eq   <- prj op = liftStruct2 (#==) <$> goAST a <*> goAST b
--         | Just And  <- prj op = liftStruct2 (#&&) <$> goAST a <*> goAST b
--         | Just Or   <- prj op = liftStruct2 (#||) <$> goAST a <*> goAST b
--         | Just Lt   <- prj op = liftStruct2 (#<)  <$> goAST a <*> goAST b
--         | Just Gt   <- prj op = liftStruct2 (#>)  <$> goAST a <*> goAST b
--         | Just Le   <- prj op = liftStruct2 (#<=) <$> goAST a <*> goAST b
--         | Just Ge   <- prj op = liftStruct2 (#>=) <$> goAST a <*> goAST b
    go (Single _) arrIx (i :* Nil)
        | Just (ArrIx arr) <- prj arrIx = do
            i' <- goSmallAST i
            return $ Single $ sugarSymPrim (ArrIx arr) i'
    go ty cond (c :* t :* f :* Nil)
        | Just Condition <- prj cond = do
            env <- ask
            case (flip runReaderT env $ goAST t, flip runReaderT env $ goAST f) of
              (t',f') -> do
                  tView <- lift $ lift $ Oper.viewT t'
                  fView <- lift $ lift $ Oper.viewT f'
                  case (tView,fView) of
                      (Oper.Return (Single tExp), Oper.Return (Single fExp)) -> do
                          c' <- goSmallAST c
                          return $ Single $ sugarSymPrim PrimCond c' tExp fExp
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
                s <- case t of
                    Single _ -> unsafeFreezeRefV state  -- For non-compound states
                    _        -> getRefV state
                s' <- localAlias iv (Single i) $
                        localAlias sv s $
                          goAST body
                setRefV state s'
             unsafeFreezeRefV state
    go (Single _) free Nil  -- TODO match
        | Just (FreeVar v) <- prj free = return $ Single $ sugarSymPrim $ FreeVar v
    go t unsPerf Nil
        | Just (UnsafePerform prog) <- prj unsPerf
        = translateExp =<< Oper.reexpressEnv unsafeTransSmallExp (Oper.liftProgram $ unComp prog)
    go t unsPerf (a :* Nil)
        | Just (UnsafePerformWith prog) <- prj unsPerf = do
            a' <- goAST a
            Oper.reexpressEnv unsafeTransSmallExp (Oper.liftProgram $ unComp prog)
            return a'

-- | Translate an expression that is assumed to fulfill @`NoPair` a@
unsafeTransSmallExp :: Monad m => Data a -> TargetT m (Prim a)
unsafeTransSmallExp a = do
    Single b <- translateExp a
    return b
  -- This function should ideally have a `NoPair a` constraint, but that is not
  -- allowed when passing it to `reexpressEnv`. It should be possible to make it
  -- work by changing the interface to `reexpressEnv`.

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

