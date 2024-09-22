{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}

module MyLib where

import Control.Concurrent
import Data.Kind (Type)
import Prelude.Linear (Ur (..))

-- * Protocol Combinators

data (:>:) :: Type -> Type -> Type

data (:<:) :: Type -> Type -> Type

data EndSesh :: Type

-- * Duality

type family Dual (p :: Type) :: Type where
  Dual (msg :>: next) = msg :<: Dual next
  Dual (msg :<: next) = msg :>: Dual next
  Dual EndSesh = EndSesh

-- * Session (Phantom) types

newtype Session channel protocol = Session (Ur channel)

class Send channel msg where
  send :: Session channel (msg :>: next) -> msg -> IO (Session channel next)

class Recv channel msg where
  recv :: Session channel (msg :<: next) %1 -> IO (msg, Session channel next)

class End channel where
  end :: Session channel EndSesh %1 -> IO ()

-- | Channel implementation
instance Send (Chan a) a where
  send (Session (Ur chan)) msg = writeChan chan msg >> pure (Session (Ur chan))

instance Recv (Chan a) a where
  recv (Session (Ur chan)) = readChan chan >>= \msg -> pure (msg, Session (Ur chan))

instance End (Chan a) where
  end (Session (Ur _)) = pure ()

type Protocol = String :>: (String :<: EndSesh)

makeSession :: forall protocol msg. IO (Session (Chan msg) protocol, Session (Chan msg) (Dual protocol))
makeSession = do
  chan <- newChan @msg
  chan' <- newChan @msg
  return (Session (Ur chan), Session (Ur chan'))

impl :: Session (Chan String) Protocol -> IO ()
impl chan = do
  chan' <- send chan "a"
  (msg, chan'') <- recv chan'
  print msg
  end chan''

revImpl :: Session (Chan String) (Dual Protocol) -> IO ()
revImpl chan = do
  (msg, chan') <- recv chan
  print msg
  chan'' <- send chan' "b"
  end chan''

foo :: IO ()
foo = do
  (sesh1, sesh2) <- makeSession @Protocol
  t1 <- forkIO $ impl sesh1
  t2 <- forkIO $ revImpl sesh2
  threadDelay 1000000000
