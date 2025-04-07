module Core.Core

import Core.Env
import Core.TT

import public Data.IORef

public export
data CaseError = DifferingArgNumbers
               | DifferingTypes
               | MatchErased (vars ** (Env Term vars, Term vars))
               | NotFullyApplied Name
               | UnknownType
public export
data Error : Type where
  CaseCompile : Name -> CaseError -> Error
  GenericMsg : String -> Error
  UndefinedName : Name -> Error

public export
Show Error where
  show (GenericMsg str) = str
  show (CaseCompile n DifferingArgNumbers)
      = "Patterns for " ++ show n ++ " have different numbers of arguments"
  show (CaseCompile n DifferingTypes)
      = "Patterns for " ++ show n ++ " require matching on different types"
  show (CaseCompile n UnknownType)
      = "Can't infer type to match in " ++ show n
  show (CaseCompile n (MatchErased (_ ** (env, tm))))
      = "Attempt to match on erased argument " ++ show tm ++
                   " in " ++ show n
  show (CaseCompile n (NotFullyApplied c))
      = "Constructor " ++ show c ++ " is not fully applied"
  show (UndefinedName x) = "Undefined name " ++ show x

public export
record Core t where
  constructor MkCore
  runCore : IO (Either Error t)

export
coreRun : Core a -> (Error -> IO b) -> (a -> IO b) -> IO b
coreRun (MkCore act) err ok
    = either err ok !act

export
coreFail : Error -> Core a
coreFail e = MkCore (pure (Left e))

export
%inline
coreLift : IO a -> Core a
coreLift op = MkCore (do op' <- op
                         pure (Right op'))

export %inline
map : (a -> b) -> Core a -> Core b
map f (MkCore a) = MkCore (map (map f) a)

export %inline
ignore : Core a -> Core ()
ignore = map (\ _ => ())

export %inline
(<$>) : (a -> b) -> Core a -> Core b
(<$>) f (MkCore a) = MkCore (map (map f) a)

-- Monad (specialised)
export %inline
(>>=) : Core a -> (a -> Core b) -> Core b
(>>=) (MkCore act) f
    = MkCore (act >>=
                   (\x => case x of
                               Left err => pure (Left err)
                               Right val => runCore (f val)))

-- Monad (specialised)
export %inline
(>>) : Core () -> Core b -> Core b
ma >> mb = ma >>= \ () => mb

-- Applicative (specialised)
export %inline
pure : a -> Core a
pure x = MkCore (pure (pure x))

export
(<*>) : Core (a -> b) -> Core a -> Core b
(<*>) (MkCore f) (MkCore a) = MkCore [| f <*> a |]

-- Control.Catchable in Idris 1, just copied here (but maybe no need for
-- it since we'll only have the one instance for Core Error...)
public export
interface Catchable (m : Type -> Type) t | m where
    throw : t -> m a
    catch : m a -> (t -> m a) -> m a

export
Catchable Core Error where
  catch (MkCore prog) h
      = MkCore ( do p' <- prog
                    case p' of
                         Left e => let MkCore he = h e in he
                         Right val => pure (Right val))
  throw = coreFail

  -- Traversable (specialised)
traverse' : (a -> Core b) -> List a -> List b -> Core (List b)
traverse' f [] acc = pure (reverse acc)
traverse' f (x :: xs) acc
    = traverse' f xs (!(f x) :: acc)

export
traverse : (a -> Core b) -> List a -> Core (List b)
traverse f xs = traverse' f xs []

export
traverse_ : (a -> Core ()) -> List a -> Core ()
traverse_ f [] = pure ()
traverse_ f (x :: xs)
    = do f x
         traverse_ f xs

namespace Binder
  export
  traverse : (a -> Core b) -> Binder a -> Core (Binder b)
  traverse f (Lam p ty) = pure $ Lam p !(f ty)
  traverse f (Pi p ty) = pure $ Pi p !(f ty)
  traverse f (PVar ty) = pure $ PVar !(f ty)
  traverse f (PVTy ty) = pure $ PVTy !(f ty)

export
data Ref : (l : label) -> Type -> Type where
      [search l]
      MkRef : IORef a -> Ref x a

export
newRef : (x : label) -> t -> Core (Ref x t)
newRef x val
  = do ref <- coreLift (newIORef val)
       pure (MkRef ref)

export %inline
get : (x : label) -> {auto ref : Ref x a} -> Core a
get x {ref = MkRef io} = coreLift (readIORef io)

export %inline
put : (x : label) -> {auto ref : Ref x a} -> a -> Core()
put x {ref = MkRef io} val = coreLift (writeIORef io val)
  
