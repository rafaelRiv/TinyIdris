module Core.Core

import public Data.IORef

export
data Error : Type where
  GenericMsg : String -> Error

export
Show Error where
  show (GenericMsg str) = str

public export
record Core t where
  constructor MkCore
  runCore : IO (Either Error t)

export
coreRun : Core a -> (Error -> IO b) -> (a -> IO b) -> IO b
coreRun (MkCore act) err ok
    = either err ok !act

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

export
traverse_ : (a -> Core ()) -> List a -> Core ()
traverse_ f [] = pure ()
traverse_ f (x :: xs)
    = do f x
         traverse_ f xs

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
  
