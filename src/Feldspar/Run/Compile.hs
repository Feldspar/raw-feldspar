{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Run.Compile where



import Control.Monad.Identity
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

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
type VExp = Struct PrimType' Prim

-- | Struct expression with hidden result type
data VExp'
  where
    VExp' :: Struct PrimType' Prim a -> VExp'

newRefV :: Monad m => TypeRep a -> String -> TargetT m (Struct PrimType' Imp.Ref a)
newRefV t base = lift $ mapStructA (const (newNamedRef base)) t

initRefV :: Monad m => String -> VExp a -> TargetT m (Struct PrimType' Imp.Ref a)
initRefV base = lift . mapStructA (initNamedRef base)

getRefV :: Monad m => Struct PrimType' Imp.Ref a -> TargetT m (VExp a)
getRefV = lift . mapStructA getRef

setRefV :: Monad m => Struct PrimType' Imp.Ref a -> VExp a -> TargetT m ()
setRefV r = lift . sequence_ . zipListStruct setRef r

unsafeFreezeRefV :: Monad m => Struct PrimType' Imp.Ref a -> TargetT m (VExp a)
unsafeFreezeRefV = lift . mapStructA unsafeFreezeRef



--------------------------------------------------------------------------------
-- * Translation environment
--------------------------------------------------------------------------------

-- | Translation environment
type Env = Map Name VExp'

-- | Add a local alias to the environment
localAlias :: MonadReader Env m
    => Name    -- ^ Old name
    -> VExp a  -- ^ New expression
    -> m b
    -> m b
localAlias v e = local (Map.insert v (VExp' e))

-- | Lookup an alias in the environment
lookAlias :: MonadReader Env m => TypeRep a -> Name -> m (VExp a)
lookAlias t v = do
    env <- ask
    return $ case Map.lookup v env of
        Nothing -> error $ "lookAlias: variable " ++ show v ++ " not in scope"
        Just (VExp' e) -> case typeEq t (toTypeRep e) of Just Dict -> e



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
    -- Assumes that `b` is not a function type
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
        = return $ mapStruct (constExp . runIdentity) $ toStruct t a
    go t lit Nil
        | Just (Literal a) <- prj lit
        = return $ mapStruct (constExp . runIdentity) $ toStruct t a
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
        | Just Pair <- prj tup = Two <$> goAST a <*> goAST b
    go t sel (ab :* Nil)
        | Just Fst <- prj sel = do
            Two a _ <- goAST ab
            return a
        | Just Snd <- prj sel = do
            Two _ b <- goAST ab
            return b
    go _ c Nil
        | Just Pi <- prj c = return $ Single $ sugarSymPrim Pi
    go _ op (a :* Nil)
        | Just Neg       <- prj op = liftStruct (sugarSymPrim Neg)       <$> goAST a
        | Just Abs       <- prj op = liftStruct (sugarSymPrim Abs)       <$> goAST a
        | Just Sign      <- prj op = liftStruct (sugarSymPrim Sign)      <$> goAST a
        | Just Exp       <- prj op = liftStruct (sugarSymPrim Exp)       <$> goAST a
        | Just Log       <- prj op = liftStruct (sugarSymPrim Log)       <$> goAST a
        | Just Sqrt      <- prj op = liftStruct (sugarSymPrim Sqrt)      <$> goAST a
        | Just Sin       <- prj op = liftStruct (sugarSymPrim Sin)       <$> goAST a
        | Just Cos       <- prj op = liftStruct (sugarSymPrim Cos)       <$> goAST a
        | Just Tan       <- prj op = liftStruct (sugarSymPrim Tan)       <$> goAST a
        | Just Asin      <- prj op = liftStruct (sugarSymPrim Asin)      <$> goAST a
        | Just Acos      <- prj op = liftStruct (sugarSymPrim Acos)      <$> goAST a
        | Just Atan      <- prj op = liftStruct (sugarSymPrim Atan)      <$> goAST a
        | Just Sinh      <- prj op = liftStruct (sugarSymPrim Sinh)      <$> goAST a
        | Just Cosh      <- prj op = liftStruct (sugarSymPrim Cosh)      <$> goAST a
        | Just Tanh      <- prj op = liftStruct (sugarSymPrim Tanh)      <$> goAST a
        | Just Asinh     <- prj op = liftStruct (sugarSymPrim Asinh)     <$> goAST a
        | Just Acosh     <- prj op = liftStruct (sugarSymPrim Acosh)     <$> goAST a
        | Just Atanh     <- prj op = liftStruct (sugarSymPrim Atanh)     <$> goAST a
        | Just Real      <- prj op = liftStruct (sugarSymPrim Real)      <$> goAST a
        | Just Imag      <- prj op = liftStruct (sugarSymPrim Imag)      <$> goAST a
        | Just Magnitude <- prj op = liftStruct (sugarSymPrim Magnitude) <$> goAST a
        | Just Phase     <- prj op = liftStruct (sugarSymPrim Phase)     <$> goAST a
        | Just Conjugate <- prj op = liftStruct (sugarSymPrim Conjugate) <$> goAST a
        | Just I2N       <- prj op = liftStruct (sugarSymPrim I2N)       <$> goAST a
        | Just I2B       <- prj op = liftStruct (sugarSymPrim I2B)       <$> goAST a
        | Just B2I       <- prj op = liftStruct (sugarSymPrim B2I)       <$> goAST a
        | Just Round     <- prj op = liftStruct (sugarSymPrim Round)     <$> goAST a
        | Just Not       <- prj op = liftStruct (sugarSymPrim Not)       <$> goAST a
        | Just BitCompl  <- prj op = liftStruct (sugarSymPrim BitCompl)  <$> goAST a
    go _ op (a :* b :* Nil)
        | Just Add     <- prj op = liftStruct2 (sugarSymPrim Add)     <$> goAST a <*> goAST b
        | Just Sub     <- prj op = liftStruct2 (sugarSymPrim Sub)     <$> goAST a <*> goAST b
        | Just Mul     <- prj op = liftStruct2 (sugarSymPrim Mul)     <$> goAST a <*> goAST b
        | Just FDiv    <- prj op = liftStruct2 (sugarSymPrim FDiv)    <$> goAST a <*> goAST b
        | Just Quot    <- prj op = liftStruct2 (sugarSymPrim Quot)    <$> goAST a <*> goAST b
        | Just Rem     <- prj op = liftStruct2 (sugarSymPrim Rem)     <$> goAST a <*> goAST b
        | Just Div     <- prj op = liftStruct2 (sugarSymPrim Div)     <$> goAST a <*> goAST b
        | Just Mod     <- prj op = liftStruct2 (sugarSymPrim Mod)     <$> goAST a <*> goAST b
        | Just Complex <- prj op = liftStruct2 (sugarSymPrim Complex) <$> goAST a <*> goAST b
        | Just Polar   <- prj op = liftStruct2 (sugarSymPrim Polar)   <$> goAST a <*> goAST b
        | Just Pow     <- prj op = liftStruct2 (sugarSymPrim Pow)     <$> goAST a <*> goAST b
        | Just Eq      <- prj op = liftStruct2 (sugarSymPrim Eq)      <$> goAST a <*> goAST b
        | Just And     <- prj op = liftStruct2 (sugarSymPrim And)     <$> goAST a <*> goAST b
        | Just Or      <- prj op = liftStruct2 (sugarSymPrim Or)      <$> goAST a <*> goAST b
        | Just Lt      <- prj op = liftStruct2 (sugarSymPrim Lt)      <$> goAST a <*> goAST b
        | Just Gt      <- prj op = liftStruct2 (sugarSymPrim Gt)      <$> goAST a <*> goAST b
        | Just Le      <- prj op = liftStruct2 (sugarSymPrim Le)      <$> goAST a <*> goAST b
        | Just Ge      <- prj op = liftStruct2 (sugarSymPrim Ge)      <$> goAST a <*> goAST b
        | Just BitAnd  <- prj op = liftStruct2 (sugarSymPrim BitAnd)  <$> goAST a <*> goAST b
        | Just BitOr   <- prj op = liftStruct2 (sugarSymPrim BitOr)   <$> goAST a <*> goAST b
        | Just BitXor  <- prj op = liftStruct2 (sugarSymPrim BitXor)  <$> goAST a <*> goAST b
        | Just ShiftL  <- prj op = liftStruct2 (sugarSymPrim ShiftL)  <$> goAST a <*> goAST b
        | Just ShiftR  <- prj op = liftStruct2 (sugarSymPrim ShiftR)  <$> goAST a <*> goAST b
    go _ arrIx (i :* Nil)
        | Just (ArrIx arr) <- prj arrIx = do
            i' <- goSmallAST i
            return $ Single $ sugarSymPrim (ArrIx arr) i'
    go ty cond (c :* t :* f :* Nil)
        | Just Cond <- prj cond = do
            env <- ask
            case (flip runReaderT env $ goAST t, flip runReaderT env $ goAST f) of
              (t',f') -> do
                  tView <- lift $ lift $ Oper.viewT t'
                  fView <- lift $ lift $ Oper.viewT f'
                  case (tView,fView) of
                      (Oper.Return (Single tExp), Oper.Return (Single fExp)) -> do
                          c' <- goSmallAST c
                          return $ Single $ sugarSymPrim Cond c' tExp fExp
                      _ -> do
                          c'  <- goSmallAST c
                          res <- newRefV ty "v"
                          ReaderT $ \env -> iff c'
                              (flip runReaderT env . setRefV res =<< t')
                              (flip runReaderT env . setRefV res =<< f')
                          unsafeFreezeRefV res
    go t divBal (a :* b :* Nil)
        | Just DivBalanced <- prj divBal
        = liftStruct2 (sugarSymPrim Quot) <$> goAST a <*> goAST b
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
    go _ free Nil
        | Just (FreeVar v) <- prj free = return $ Single $ sugarSymPrim $ FreeVar v
    go t unsPerf Nil
        | Just (UnsafePerform prog) <- prj unsPerf
        = translateExp =<<
            Oper.reexpressEnv unsafeTransSmallExp (Oper.liftProgram $ unComp prog)
    go t unsPerf (a :* Nil)
        | Just (UnsafePerformWith prog) <- prj unsPerf = do
            a' <- goAST a
            Oper.reexpressEnv unsafeTransSmallExp (Oper.liftProgram $ unComp prog)
            return a'
    go _ s _ = error $ "translateExp: no handling of symbol " ++ renderSym s

-- | Translate an expression that is assumed to fulfill @`PrimType` a@
unsafeTransSmallExp :: Monad m => Data a -> TargetT m (Prim a)
unsafeTransSmallExp a = do
    Single b <- translateExp a
    return b
  -- This function should ideally have a `PrimType' a` constraint, but that is
  -- not allowed when passing it to `reexpressEnv`. It should be possible to
  -- make it work by changing the interface to `reexpressEnv`.

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
-- > cc -std=c99 YOURPROGRAM.c
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
-- > cc -std=c99 YOURPROGRAM.c
compileAll :: MonadRun m => m a -> [(String, String)]
compileAll = Imp.compileAll . translate . liftRun

-- | Compile a program to C code and print it on the screen. To compile the
-- resulting C code, use something like
--
-- > cc -std=c99 YOURPROGRAM.c
icompile :: MonadRun m => m a -> IO ()
icompile = Imp.icompile . translate . liftRun

-- | Generate C code and use CC to check that it compiles (no linking)
compileAndCheck' :: MonadRun m => ExternalCompilerOpts -> m a -> IO ()
compileAndCheck' opts = Imp.compileAndCheck' opts . translate . liftRun

-- | Generate C code and use CC to check that it compiles (no linking)
compileAndCheck :: MonadRun m => m a -> IO ()
compileAndCheck = compileAndCheck' mempty

-- | Generate C code, use CC to compile it, and run the resulting executable
runCompiled' :: MonadRun m => ExternalCompilerOpts -> m a -> IO ()
runCompiled' opts = Imp.runCompiled' opts . translate . liftRun

-- | Generate C code, use CC to compile it, and run the resulting executable
runCompiled :: MonadRun m => m a -> IO ()
runCompiled = runCompiled' mempty

-- | Compile a program and make it available as an 'IO' function from 'String'
-- to 'String' (connected to @stdin@/@stdout@. respectively). Note that
-- compilation only happens once, even if the 'IO' function is used many times
-- in the body.
withCompiled' :: MonadRun m
    => ExternalCompilerOpts
    -> m a  -- ^ Program to compile
    -> ((String -> IO String) -> IO b)
         -- ^ Function that has access to the compiled executable as a function
    -> IO b
withCompiled' opts = Imp.withCompiled' opts . translate . liftRun

-- | Compile a program and make it available as an 'IO' function from 'String'
-- to 'String' (connected to @stdin@/@stdout@. respectively). Note that
-- compilation only happens once, even if the 'IO' function is used many times
-- in the body.
withCompiled :: MonadRun m
    => m a  -- ^ Program to compile
    -> ((String -> IO String) -> IO b)
         -- ^ Function that has access to the compiled executable as a function
    -> IO b
withCompiled = withCompiled' mempty {externalSilent = True}

-- | Like 'runCompiled'' but with explicit input/output connected to
-- @stdin@/@stdout@. Note that the program will be compiled every time the
-- function is applied to a string. In order to compile once and run many times,
-- use the function 'withCompiled''.
captureCompiled' :: MonadRun m
    => ExternalCompilerOpts
    -> m a        -- ^ Program to run
    -> String     -- ^ Input to send to @stdin@
    -> IO String  -- ^ Result from @stdout@
captureCompiled' opts = Imp.captureCompiled' opts . translate . liftRun

-- | Like 'runCompiled' but with explicit input/output connected to
-- @stdin@/@stdout@. Note that the program will be compiled every time the
-- function is applied to a string. In order to compile once and run many times,
-- use the function 'withCompiled'.
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

