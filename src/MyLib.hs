{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeFamilies #-}

module MyLib where

import Control.Concurrent
import Control.Functor.Linear qualified as Linear
import Data.Kind (Type)
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L
import System.IO.Linear qualified as System

-- * Protocol Combinators

data (:>:) msg next

data (:<:) msg next

data (:+:) p1 p2

-- data Choose ps next

data (:*:) p1 p2

-- data Offer ps next

data EndSesh :: Type

-- * Duality

type family Dual (p :: Type) :: Type where
  Dual (msg :>: next) = msg :<: Dual next
  Dual (msg :<: next) = msg :>: Dual next
  Dual (p1 :+: p2) = Dual p1 :*: Dual p2
  Dual (p1 :*: p2) = Dual p1 :+: Dual p2
  Dual EndSesh = EndSesh

-- * Session (Phantom) types

newtype Session channel protocol = Session (Ur channel)

class SendClass channel msg where
  send :: Session channel (msg :>: next) %1 -> msg -> System.IO (Session channel next)

class RecvClass channel msg where
  recv :: Session channel (msg :<: next) %1 -> System.IO (Ur msg, Session channel next)

class OrClass channel p1 p2 where
  or ::
    Session channel (p1 :+: p2) %1 ->
    Either (Session channel p1 %1 -> a) (Session channel p2 %1 -> a) ->
    a

class And channel p1 p2 where
  and :: Session channel (p1 :*: p2) %1 -> System.IO (Session channel p1, Session channel p2)

class End channel where
  end :: Session channel EndSesh %1 -> System.IO ()

-- | Channel implementation
instance SendClass (Chan a) a where
  send (Session (Ur chan)) msg =
    System.fromSystemIOU (writeChan chan msg) Linear.>>= \(Ur _) -> Linear.pure (Session (Ur chan))

instance RecvClass (Chan a) a where
  recv (Session (Ur chan)) = System.fromSystemIOU (readChan chan) Linear.>>= \msg -> Linear.pure (msg, Session (Ur chan))

instance OrClass (Chan a) p1 p2 where
  or (Session (Ur channel)) (Left f) = f (Session (Ur channel))
  or (Session (Ur channel)) (Right g) = g (Session (Ur channel))

instance And (Chan a) p1 p2 where
  and (Session (Ur chan)) = Linear.pure (Session (Ur chan), Session (Ur chan))

instance End (Chan a) where
  end (Session (Ur _)) = Linear.pure ()

type Protocol = String :>: (String :<: (EndSesh :+: (String :<: EndSesh)))

-- makeSession :: forall protocol msg. IO (Session (Chan msg) protocol, Session (Chan msg) (Dual protocol))
-- makeSession = do
--   chan <- newChan @msg
--   chan' <- newChan @msg
--   return (Session (Ur chan), Session (Ur chan'))

impl :: Session (Chan String) Protocol %1 -> System.IO ()
impl chan = Linear.do
  chan' <- send chan "hi"
  (Ur msg, chan'') <- recv chan'
  System.fromSystemIO (print msg)
  case length msg of
    0 -> MyLib.or chan'' L.$ Left end
    _ -> MyLib.or chan'' L.$
      Right $
        \chan''' -> Linear.do
          (Ur msg', chan'''') <- recv chan'''
          System.fromSystemIO (print msg')
          end chan''''

revImpl :: Session (Chan String) (Dual Protocol) %1 -> System.IO ()
revImpl chan = Linear.do
  (Ur msg, chan') <- recv chan
  System.fromSystemIO (print msg)
  chan'' <- send chan' "Received"
  (chan1, chan2) <- MyLib.and chan''
  end chan1
  chan3 <- send chan2 "woo"
  end chan3
