{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Prelude                 hiding (read)

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Either.Combinators (swapEither)
import qualified Data.Map.Strict         as M
import           GHC.Generics            (Generic)

import           EitherT
import           Orphans                 ()

type M =
    ReaderT Env (
        ContT Value (
            StateT Store (
                WriterT String (
                    ExceptT Error []
                )
            )
        )
    )

type Name = String

type Error = String

type Fun = M Value -> M Value

newtype Loc = Loc { getLoc :: Integer }
    deriving (Generic, Eq, Ord, Show)
instance NFData Loc

data Value
    = IntVal !Integer
    | LocVal !Loc
    | FunVal !Fun
    | NoVal
    deriving Generic
instance NFData Value

instance Show Value where
    show (IntVal i) = "IntVal " ++ show i
    show (LocVal l) = "LocVal " ++ show l
    show (FunVal _) = "FunVal ????"
    show NoVal      = "NoVal"

newtype Env = Env { getEnv :: M.Map Name (M Value) }

data Store = Store
    { counter :: !Integer
    , store   :: !(M.Map Loc (M Value))
    }
    deriving Show
instance Show (M Value) where
    show m = show $ runMValue m

runMValue :: M Value -> Value
runMValue = either (fst . fst) (error . unlines) . traverse swapEither . runM

runM :: M Value -> [Either Error ((Value, Store), String)]
runM m = results
  where
    env = Env M.empty
    store = Store { counter = 0, store = M.empty }
    contT = runReaderT m env
    stateT = runContT contT return
    writerT = runStateT stateT store
    exceptT = runWriterT writerT
    results = runExceptT exceptT

makeLensesFor [("counter", "counterLens"), ("store", "storeLens")] ''Store

data Term
    = A TermA
    | F TermF
    | R TermR
    | L TermL
    | T TermT
    | C TermC
    | N TermN
    deriving Show

data TermA
    = I Integer
    | Plus Term Term
    deriving Show

data TermF
    = Var Name
    | Abs Name Term
    | AppN Term Term
    | AppV Term Term
    deriving Show

data TermL = AppL Term Term
    deriving Show

data TermC = Callcc
    deriving Show

data TermR
    = Assign Term Term
    | Seq Term Term
    | Ref Term
    | Deref Term
    deriving Show

data TermT = Trace String Term
    deriving Show

data TermN = Amb [Term]
    deriving Show

read :: Loc -> M Value
read l = do
    store <- use storeLens
    case M.lookup l store of
        Nothing -> err $ "Can't find " ++ show l
        Just v  -> v

write :: Loc -> M Value -> M ()
write l v = storeLens %= M.insert l v

alloc :: M Loc
alloc = do
    nextLoc <- uses counterLens Loc
    counterLens %= (+1)
    storeLens %= M.insert nextLoc (return $ IntVal 0)
    return nextLoc

class Interp t where
    interp :: t -> M Value

instance Interp Term where
    interp (A t) = interp t
    interp (F t) = interp t
    interp (L t) = interp t
    interp (C t) = interp t
    interp (R t) = interp t
    interp (T t) = interp t
    interp (N t) = interp t

instance Interp TermA where
    interp (I i) = return $ IntVal i
    interp (Plus t1 t2) = do
        IntVal i1 <- interp t1
        IntVal i2 <- interp t2
        return $ IntVal $ i1 + i2

instance Interp TermF where
    interp (Var var) = do
        Env env <- ask
        case M.lookup var env of
            Nothing -> err $ "Can't find " ++ var
            Just x  -> x
    interp (Abs v e) =
        return $ FunVal $ \c -> local (Env . M.insert v c . getEnv) (interp e)
    interp (AppN e1 e2) = do
        FunVal f <- interp e1
        env <- ask
        f $ local (const env) $ interp e2
    interp (AppV e1 e2) = do
        FunVal f <- interp e1
        v <- interp e2
        f $ return v

instance Interp TermL where
    interp (AppL e1 e2) = do
        FunVal f <- interp e1
        l <- alloc
        env <- ask
        let
            thunk = do
                v <- local (const env) $ interp e2
                write l $ return v
                return v
        write l thunk
        f $ read l

instance Interp TermC where
    interp Callcc = return $ FunVal $ \f -> do
        FunVal f' <- f
        callCC $ \k -> f' (return $ FunVal (\a -> a >>= k))

instance Interp TermR where
    interp (Ref e) = do
        v <- interp e
        l <- alloc
        write l $ return v
        return $ LocVal l
    interp (Deref e) = do
        LocVal l <- interp e
        read l
    interp (Assign e1 e2) = do
        LocVal l <- interp e1
        v <- interp e2
        write l $ return v
        return NoVal
    interp (Seq e1 e2) = do
        v <- interp e1
        w <- interp e2
        return $ deepseq v w

instance Interp TermT where
    interp (Trace l e) = do
        tell $ "enter " ++ l ++ "\n"
        v <- interp e
        tell $ "leave " ++ l ++ "\n"
        return v

instance Interp TermN where
    -- TODO: MonadList class with a merge function to generalize this join lift
    -- lift lift garbage
    interp (Amb choices) =
        join $ (lift . lift . lift . lift . lift) $ map interp choices
