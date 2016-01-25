module Feldspar.Compile.Software where

import Control.Applicative ((<$>))
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

import Language.Embedded.CExp (CType)
import qualified Language.Embedded.CExp           as Soft
import qualified Language.Embedded.Imperative     as Soft
import qualified Language.Embedded.Imperative.CMD as Soft
import qualified Language.Embedded.Backend.C      as Soft

import Data.VirtualContainer

import Feldspar.Representation hiding (Program)
import Feldspar.Optimize
import Feldspar.Compile.Lower
import qualified Feldspar.Representation as Feld
import qualified Feldspar.Frontend       as Feld

--------------------------------------------------------------------------------
-- * .
--------------------------------------------------------------------------------

-- | Virtual expression
type VExp = Virtual SmallType Soft.CExp

-- | Virtual expression with hidden result type
data VExp_
  where
    VExp_ :: Type a => Virtual SmallType Soft.CExp a -> VExp_

type TargetCMD =
        Soft.RefCMD     Soft.CExp
  H.:+: Soft.ArrCMD     Soft.CExp
  H.:+: Soft.ControlCMD Soft.CExp
  H.:+: Soft.FileCMD    Soft.CExp
  H.:+: Soft.CallCMD    Soft.CExp
  H.:+: Soft.ObjectCMD  Soft.CExp
  
type Env = Map Name VExp_

-- | Target monad for translation
type Target = ReaderT Env (H.Program TargetCMD)

--------------------------------------------------------------------------------

newRefV :: VirtualType SmallType a => Target (Virtual SmallType Soft.Ref a)
newRefV = Reader.lift $ mapVirtualA (const Soft.newRef) virtRep

initRefV :: VirtualType SmallType a => VExp a -> Target (Virtual SmallType Soft.Ref a)
initRefV = Reader.lift . mapVirtualA (Soft.initRef)

getRefV :: VirtualType SmallType a => Virtual SmallType Soft.Ref a -> Target (VExp a)
getRefV = Reader.lift . mapVirtualA (Soft.getRef)

setRefV :: VirtualType SmallType a => Virtual SmallType Soft.Ref a -> VExp a -> Target ()
setRefV r = Reader.lift . sequence_ . zipListVirtual (Soft.setRef) r

unsafeFreezeRefV :: VirtualType SmallType a => Virtual SmallType Soft.Ref a -> Target (VExp a)
unsafeFreezeRefV = Reader.lift . mapVirtualA (Soft.unsafeFreezeRef)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | Translate instructions to the 'Target' monad
class Lower instr
  where
    lowerInstr :: instr Target a -> Target a

instance Lower (Soft.RefCMD Data)
  where
    lowerInstr (Soft.NewRef)     = Reader.lift (Soft.newRef)
    lowerInstr (Soft.InitRef a)  = Reader.lift . Soft.initRef =<< translateSmallExp a
    lowerInstr (Soft.SetRef r a) = Reader.lift . Soft.setRef r =<< translateSmallExp a
    lowerInstr (Soft.GetRef r)          = fmap liftVar $ Reader.lift $ Soft.getRef r
    lowerInstr (Soft.UnsafeFreezeRef r) = fmap liftVar $ Reader.lift $ Soft.unsafeFreezeRef r

instance Lower (Soft.ArrCMD Data)
  where
    lowerInstr (Soft.NewArr n)       = Reader.lift . Soft.newArr =<< translateSmallExp n
    lowerInstr (Soft.NewArr_)        = Reader.lift   Soft.newArr_
    lowerInstr (Soft.GetArr i arr)   = do
        i' <- translateSmallExp i
        fmap liftVar $ Reader.lift $ Soft.getArr i' arr
    lowerInstr (Soft.SetArr i a arr) = do
        i' <- translateSmallExp i
        a' <- translateSmallExp a
        Reader.lift $ Soft.setArr i' a' arr
    lowerInstr (Soft.CopyArr dst src n) =
        Reader.lift . Soft.copyArr dst src =<< translateSmallExp n

instance Lower (Soft.ControlCMD Data)
  where
    lowerInstr (Soft.If c t f) = do
        c' <- translateSmallExp c
        ReaderT $ \env -> Soft.iff c'
            (flip runReaderT env t)
            (flip runReaderT env f)
    lowerInstr (Soft.While cont body) = do
        ReaderT $ \env -> Soft.while
            (flip runReaderT env $ translateSmallExp =<< cont)
            (flip runReaderT env body)
    lowerInstr (Soft.For (lo,step,hi) body) = do
        lo' <- translateSmallExp lo
        hi' <- traverse translateSmallExp hi
        ReaderT $ \env -> Soft.for
            (lo',step,hi')
            (flip runReaderT env . body . liftVar)
    lowerInstr (Soft.Assert cond msg) = do
        cond' <- translateSmallExp cond
        Reader.lift $ Soft.assert cond' msg
    lowerInstr (Soft.Break) = Reader.lift Soft.break

instance Lower (Soft.FileCMD Data)
  where
    lowerInstr (Soft.FOpen file mode)   = Reader.lift $ Soft.fopen file mode
    lowerInstr (Soft.FClose h)          = Reader.lift $ Soft.fclose h
    lowerInstr (Soft.FPrintf h form as) = Reader.lift . Soft.fprf h form . reverse =<< transPrintfArgs as
    lowerInstr (Soft.FEof h)            = fmap liftVar $ Reader.lift $ Soft.feof h
    lowerInstr (Soft.FGet h)            = fmap liftVar $ Reader.lift $ Soft.fget h

instance Lower (Soft.ObjectCMD Data)
  where
    lowerInstr (Soft.NewObject t) =
        Reader.lift $ Soft.newObject t
    lowerInstr (Soft.InitObject name True t as) = do
        Reader.lift . Soft.initObject name t =<< transFunArgs as
    lowerInstr (Soft.InitObject name False t as) = do
        Reader.lift . Soft.initUObject name t =<< transFunArgs as

instance Lower (Soft.CallCMD Data)
  where
    lowerInstr (Soft.AddInclude incl)    = Reader.lift $ Soft.addInclude incl
    lowerInstr (Soft.AddDefinition def)  = Reader.lift $ Soft.addDefinition def
    lowerInstr (Soft.AddExternProc p as) = Reader.lift . Soft.addExternProc p =<< transFunArgs as
    lowerInstr (Soft.CallProc p as)      = Reader.lift . Soft.callProc p      =<< transFunArgs as
    lowerInstr (Soft.CallFun f as)       = fmap liftVar . Reader.lift . Soft.callFun f =<< transFunArgs as
    lowerInstr (Soft.AddExternFun f (_ :: proxy (Data res)) as) =
        Reader.lift . Soft.addExternFun f (Proxy :: Proxy (Soft.CExp res)) =<< transFunArgs as

instance (Lower i1, Lower i2) => Lower (i1 H.:+: i2)
  where
    lowerInstr (H.Inl i) = lowerInstr i
    lowerInstr (H.Inr i) = lowerInstr i

-- | Lift a 'CExp' that has been created using
-- 'Language.Embedded.Expression.litExp' or
-- 'Language.Embedded.Expression.varExp'
liftVar :: SmallType a => Soft.CExp a -> Data a
liftVar (Soft.CExp (Sym (Soft.T (Soft.Var v))))   = Data $ Sym $ (inj (FreeVar v) :&: typeRep)
liftVar (Soft.CExp (Sym (Soft.T (Soft.Lit _ a)))) = Feld.value a

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
        Nothing | Right Dict <- pwit pCType tr
               -> error $ "lookAlias: variable " ++ show v ++ " not in scope"
        Just (VExp_ e) -> case gcast pFeldTypes e of
            Left msg -> error $ "lookAlias: " ++ msg
            Right e' -> e'
  where
    tr = typeRep :: TypeRep FeldTypes a

-- | ...
transPrintfArgs :: [Soft.PrintfArg Data] -> Target [Soft.PrintfArg Soft.CExp]
transPrintfArgs = mapM $ \(Soft.PrintfArg a) -> Soft.PrintfArg <$> translateSmallExp a

-- | ...
transFunArgs :: [Soft.FunArg Data] -> Target [Soft.FunArg Soft.CExp]
transFunArgs = mapM $ Soft.mapMArg predCast translateSmallExp
  where
    predCast :: Soft.VarPredCast Data Soft.CExp
    predCast _ a = a

--------------------------------------------------------------------------------

-- | Translate a Feldspar program to the 'Target' monad
lower :: H.Program Feld.CMD a -> Target a
lower = H.interpretWithMonad lowerInstr

-- | Translate a Feldspar program into a program that uses 'TargetCMD'
lowerTop :: Feld.Program a -> H.Program TargetCMD a
lowerTop = flip runReaderT Map.empty . lower . unProgram

-- | Translate a Software program into a program that uses 'TargetCMD'
lowerSoft :: Feld.Software a -> H.Program TargetCMD a
lowerSoft = flip runReaderT Map.empty . interp2 . unSoftware
  where
    interp2 :: (H.HFunctor i, Lower i, H.HFunctor j, Lower j) => H.ProgramT i (H.Program j) a -> Target a
    interp2 = H.interpretWithMonadT lowerInstr (H.interpretWithMonad lowerInstr)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | ...
transAST :: ASTF FeldDomain a -> Target (VExp a)
transAST = goAST . optimize
  where
    goAST :: ASTF FeldDomain a -> Target (VExp a)
    goAST = simpleMatch (\(s :&: t) -> go t s)

    goSmallAST :: SmallType a => ASTF FeldDomain a -> Target (Soft.CExp a)
    goSmallAST = fmap viewActual . goAST

    go :: TypeRep FeldTypes (DenResult sig)
       -> FeldConstructs sig
       -> Args (AST FeldDomain) sig
       -> Target (VExp (DenResult sig))
    go t lit Nil
        | Just (Literal a) <- prj lit
        , Right Dict <- pwit pType t
        = return $ mapVirtual (Soft.value . runIdentity) $ toVirtual a
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
        | Just I2N <- prj op = liftVirt Soft.i2n  <$> goAST a
        | Just Not <- prj op = liftVirt Soft.not_ <$> goAST a
    go t op (a :* b :* Nil)
        | Just Add <- prj op = liftVirt2 (+)        <$> goAST a <*> goAST b
        | Just Sub <- prj op = liftVirt2 (-)        <$> goAST a <*> goAST b
        | Just Mul <- prj op = liftVirt2 (*)        <$> goAST a <*> goAST b
        | Just Eq  <- prj op = liftVirt2 (Soft.#==) <$> goAST a <*> goAST b
        | Just Lt  <- prj op = liftVirt2 (Soft.#<)  <$> goAST a <*> goAST b
        | Just Gt  <- prj op = liftVirt2 (Soft.#>)  <$> goAST a <*> goAST b
        | Just Le  <- prj op = liftVirt2 (Soft.#<=) <$> goAST a <*> goAST b
        | Just Ge  <- prj op = liftVirt2 (Soft.#>=) <$> goAST a <*> goAST b
    go ty cond (c :* t :* f :* Nil)
        | Just Condition <- prj cond = do
            env <- Reader.ask
            case (flip runReaderT env $ goAST t, flip runReaderT env $ goAST f) of
              (t',f') | H.Return (Actual t'') <- H.view t'
                      , H.Return (Actual f'') <- H.view f'
                      -> do c' <- goSmallAST c
                            return $ Actual (c' Soft.? t'' $ f'')
              (t',f') -> do
                  c'  <- goSmallAST c
                  res <- newRefV
                  ReaderT $ \env -> Soft.iff c'
                      (flip runReaderT env . setRefV res =<< t')
                      (flip runReaderT env . setRefV res =<< f')
                  unsafeFreezeRefV res
    go t loop (len :* init :* (lami :$ (lams :$ body)) :* Nil)
        | Just ForLoop   <- prj loop
        , Just (LamT iv) <- prj lami
        , Just (LamT sv) <- prj lams
        = do len'  <- goSmallAST len
             state <- initRefV =<< goAST init
             ReaderT $ \env -> Soft.for (0, 1, Soft.Excl len') $ \i -> flip runReaderT env $ do
                s <- case pwit pSmallType t of
                    Right Dict -> unsafeFreezeRefV state  -- For non-compound states
                    _          -> getRefV state
                s' <- localAlias iv (Actual i) $
                        localAlias sv s $
                          goAST body
                setRefV state s'
             unsafeFreezeRefV state
    go t free Nil
        | Just (FreeVar v) <- prj free = return $ Actual $ Soft.variable v
    go t arrIx (i :* Nil)
        | Just (UnsafeArrIx arr) <- prj arrIx = do
            i' <- goSmallAST i
            fmap Actual $ Reader.lift $ Soft.getArr i' arr
    go t unsPerf Nil
        | Just (UnsafePerform prog) <- prj unsPerf
        = translateExp =<< lower (unProgram prog)
    go t unsPerf (a :* Nil)
        | Just (UnsafePerformWith prog) <- prj unsPerf = do
            a' <- goAST a
            lower (unProgram prog)
            return a'

-- | Translate a Feldspar expression
translateExp :: Data a -> Target (VExp a)
translateExp = transAST . unData

-- | Translate a Feldspar expression that can be represented as a simple 'CExp'
translateSmallExp :: SmallType a => Data a -> Target (Soft.CExp a)
translateSmallExp = fmap viewActual . translateExp

--------------------------------------------------------------------------------
-- * Run.
--------------------------------------------------------------------------------

-- | Interpret a program in the 'IO' monad
runIO :: Feld.Program a -> IO a
runIO = H.interpret . lowerTop

-- | Compile a program to C code represented as a string. To compile the
-- resulting C code, use something like
--
-- > gcc -std=c99 YOURPROGRAM.c
compile :: Feld.Program a -> String
compile  = Soft.compile . lowerTop
compile2 = Soft.compile . lowerSoft -- !

-- | Compile a program to C code and print it on the screen. To compile the
-- resulting C code, use something like
--
-- > gcc -std=c99 YOURPROGRAM.c
icompile :: Feld.Program a -> IO ()
icompile  = putStrLn . compile
icompile2 = putStrLn . compile2  -- !

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck' :: Soft.ExternalCompilerOpts -> Feld.Program a -> IO ()
compileAndCheck' opts = Soft.compileAndCheck' opts . lowerTop

-- | Generate C code and use GCC to check that it compiles (no linking)
compileAndCheck :: Feld.Program a -> IO ()
compileAndCheck = compileAndCheck' mempty

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled' :: Soft.ExternalCompilerOpts -> Feld.Program a -> IO ()
runCompiled' opts = Soft.runCompiled' opts . lowerTop

-- | Generate C code, use GCC to compile it, and run the resulting executable
runCompiled :: Feld.Program a -> IO ()
runCompiled = runCompiled' mempty

-- | Like 'runCompiled'' but with explicit input/output connected to
-- @stdin@/@stdout@
captureCompiled'
    :: Soft.ExternalCompilerOpts
    -> Feld.Program a  -- ^ Program to run
    -> String          -- ^ Input to send to @stdin@
    -> IO String       -- ^ Result from @stdout@
captureCompiled' opts = Soft.captureCompiled' opts . lowerTop

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
    :: Soft.ExternalCompilerOpts
    -> Feld.Program a  -- ^ Program to run
    -> String          -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled' opts = Soft.compareCompiled' opts . lowerTop

-- | Compare the content written to 'stdout' from interpretation in 'IO' and
-- from running the compiled C code
compareCompiled
    :: Feld.Program a  -- ^ Program to run
    -> String          -- ^ Input to send to @stdin@
    -> IO ()
compareCompiled = compareCompiled' mempty

--------------------------------------------------------------------------------
-- Stuff
--------------------------------------------------------------------------------

pCType :: Proxy CType
pCType = Proxy

--------------------------------------------------------------------------------