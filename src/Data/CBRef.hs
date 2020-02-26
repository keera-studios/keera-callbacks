-- |
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Data.CBRef
   ( CBRef
   , newCBRef
   , readCBRef
   , writeCBRef
   , modifyCBRef
   , atomicModifyCBRef
   , installCallbackCBRef
   )
  where

import Data.IORef

data CBRef a = CBRef (IORef (a, [IO ()]))

newCBRef :: a -> IO (CBRef a)
newCBRef v = fmap CBRef $ newIORef (v,[])

readCBRef :: CBRef a -> IO a
readCBRef (CBRef ref) = fmap fst $ readIORef ref

writeCBRef :: CBRef a -> a -> IO ()
writeCBRef (CBRef ref) v = do
  modifyIORef ref (\(_,cb) -> (v,cb))
  runCallbacks ref

runCallbacks :: IORef (a, [IO ()]) -> IO ()
runCallbacks ref = (sequence_.snd) =<< readIORef ref

modifyCBRef :: CBRef a -> (a -> a) -> IO ()
modifyCBRef (CBRef ref) f = do
 modifyIORef ref (\(v,cb) -> (f v,cb))
 runCallbacks ref

atomicModifyCBRef :: CBRef a -> (a -> (a, b)) -> IO b
atomicModifyCBRef (CBRef ref) f = do
  v <- atomicModifyIORef ref modifier
  runCallbacks ref
  return v
 where modifier (v,cb) = let (v', r) = f v in ((v',cb), r)

installCallbackCBRef :: CBRef a -> IO () -> IO ()
installCallbackCBRef (CBRef ref) p =
  modifyIORef ref (\(v,cb) -> (v,cb ++ [p]))
