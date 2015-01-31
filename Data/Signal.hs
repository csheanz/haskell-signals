module Utils.Data.Signal
    ( Signal()
    , Connection()
    , newSignal, connect, disconnect, block, unblock
    , withConnection, withBlock, withUnblock
    , emit, emit_
    ) where

import Control.Applicative
import Control.Exception (bracket, bracket_)
import Control.Monad
import qualified Data.Foldable
import Data.Maybe
import qualified Data.Traversable
import Data.IORef
import qualified Data.Map as M

-- first slot value
-- set to minBound::Int to double the number of ops required
-- to switch to large int.
slot0 :: Integer
slot0 = fromIntegral (minBound :: Int)

newtype Counter = Counter (IORef Int)
newCounter = Counter <$> newIORef 0
inc (Counter ref) = atomicModifyIORef ref $ \i -> let j = succ i in (j, j)
dec (Counter ref) = atomicModifyIORef ref $ \i -> let j = pred i in (j, j)
gtz (Counter ref) = (> 0) <$> readIORef ref

data Slot a = Slot Counter a
data Sig a = Sig Integer (M.Map Integer (Slot a))

addSig (Sig i m) a =
    let m' = M.insert i a m
        i' = succ i
    in  (Sig i' m', i)

delSig (Sig i m) x = Sig i (M.delete x m)

newtype Signal a = Signal' (IORef (Sig a))

data Connection = Connection'
    { disconnect :: IO ()
    , block :: IO ()
    , unblock :: IO ()
    }

newSignal :: IO (Signal a)
newSignal = do
    ref <- newIORef $ Sig slot0 M.empty
    return $ Signal' ref

connect :: Signal a -> a -> IO Connection
connect (Signal' ref) a = do
    cnt <- newCounter
    let slot = Slot cnt a
    atomicModifyIORef ref $ \sig ->
        let (s', i) = addSig sig slot
            disconn = atomicModifyIORef ref $ \sig' -> (delSig sig' i, ())
            blk = void $ inc cnt
            unblk = void $ dec cnt
        in  (s', Connection' disconn blk unblk)

withConnection :: Signal a -> a -> IO a -> IO a
withConnection sig a = bracket (connect sig a) (disconnect) . const

withBlock :: Connection -> IO a -> IO a
withBlock c = bracket_ (block c) (unblock c)

withUnblock :: Connection -> IO a -> IO a
withUnblock c = bracket_ (unblock c) (block c)

emit :: (a -> IO b) -> Signal a -> IO [b]
emit z (Signal' ref) = do
    Sig _ m <- readIORef ref
    go $ M.toList m
    where
        go [] = return []
        go ((_, Slot x f) : ss) = do
            blocked <- gtz x
            if blocked
                then go ss
                else do
                    a <- z f
                    b <- go ss
                    return (a:b)

emit_ :: (a -> IO b) -> Signal a -> IO ()
emit_ z (Signal' ref) = do
    Sig _ m <- readIORef ref
    Data.Foldable.mapM_ go m
    where
        go (Slot x f) = do
            blocked <- gtz x
            when (not blocked) (z f >> return ())
