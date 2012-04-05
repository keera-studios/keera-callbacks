module Data.CBMVar
   ( CBMVar
   , newCBMVar
   , readCBMVar
   , writeCBMVar
   , modifyCBMVar
   , installCallbackCBMVar
   )
  where

import Control.Concurrent.MVar

data CBMVar a = CBMVar (MVar (a, [IO ()]))

newCBMVar :: a -> IO (CBMVar a)
newCBMVar v = fmap CBMVar $ newMVar (v,[])

readCBMVar :: CBMVar a -> IO a
readCBMVar (CBMVar ref) = fmap fst $ readMVar ref

writeCBMVar :: CBMVar a -> a -> IO ()
writeCBMVar (CBMVar ref) v = do
  modifyMVar_ ref (\(_,cb) -> return (v,cb))
  runCallbacks ref

runCallbacks :: MVar (a, [IO ()]) -> IO ()
runCallbacks ref = (sequence_.snd) =<< readMVar ref

modifyCBMVar :: CBMVar a -> (a -> IO a) -> IO ()
modifyCBMVar (CBMVar ref) f = do
 modifyMVar_ ref (\(v,cb) -> f v >>= \v' -> return (v',cb))
 runCallbacks ref

installCallbackCBMVar :: CBMVar a -> IO () -> IO ()
installCallbackCBMVar (CBMVar ref) p =
  modifyMVar_ ref (\(v,cb) -> return (v,cb ++ [p]))
