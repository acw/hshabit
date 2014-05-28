module Compiler.Monad(
         CompilerM
       , runCompiler
       , warn
       , err
       )
 where

import Compiler.Error
import Compiler.Options
import Compiler.Warning
import Control.Applicative
import MonadLib

newtype Monad m => CompilerM m a = CM
    (WriterT [Warning]
      (WriterT [Error]
        (ReaderT Options m)) a)
 deriving (Applicative, Functor, Monad)

instance Monad m => BaseM (CompilerM m) (CompilerM m) where inBase = id

runCompiler :: Monad m => Options -> CompilerM m a -> m (a, [Warning], [Error])
runCompiler opts (CM m) =
  do ((res, warns), errs) <- runReaderT opts $ runWriterT $ runWriterT m
     if optWarnErrors opts
        then return (res, [], errs ++ map ErrorWarning warns)
        else return (res, warns, errs)

warn :: Monad m => Warning -> CompilerM m ()
warn w = CM $ put [w]

err :: Monad m => Error -> CompilerM m ()
err e = CM $ lift $ put [e]


