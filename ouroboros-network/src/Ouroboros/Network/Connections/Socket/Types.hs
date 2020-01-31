{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Connections.Socket.Types
  ( SockType (..)
  , SockAddr (..)
  , ConnectionId (..)
  , makeConnectionId
  , connectionIdPair
  , Some (..)
  , someSockType
  , withSockType
  , matchSockType
  , matchSockAddr
  , forgetSockType
  ) where

import Data.Kind (Type)
import qualified Network.Socket as Socket

data SockType where
  IPv4 :: SockType
  IPv6 :: SockType
  Unix :: SockType

-- | Like `Network.Socket.SockAddr` but with a type parameter indicating which
-- kind of address it is: IPv4, IPv6, or Unix domain.
data SockAddr (sockType :: SockType) where
  SockAddrIPv4 :: !Socket.PortNumber -> !Socket.HostAddress -> SockAddr IPv4
  SockAddrIPv6 :: !Socket.PortNumber
               -> !Socket.FlowInfo
               -> !Socket.HostAddress6
               -> !Socket.ScopeID
               -> SockAddr IPv6
  -- No strictness here because the Network.Socket analogue is not strict.
  SockAddrUnix :: String -> SockAddr Unix

instance Eq (SockAddr sockType) where
  x == y = forgetSockType x == forgetSockType y

instance Show (SockAddr sockType) where
  show = show . forgetSockType

-- | A connection is identified by a pair of addresses.
-- The first (left) address is the bound address of the socket, and the second
-- is the remote address.
--
-- For IPv* this is fine, but not for Unix domain sockets: these can be unnamed,
-- so that a connecting socket which does not bind will get the address
-- `SockAddrUnix ""`.
--
-- How to deal with this?
-- One obvious way is to simply make up an ephemeral identifier for any
-- `SockAddrUnix ""` term, maybe using its file descriptor.
--
data ConnectionId where
  ConnectionIdIPv6 :: !(SockAddr IPv6) -> !(SockAddr IPv6) -> ConnectionId
  ConnectionIdIPv4 :: !(SockAddr IPv4) -> !(SockAddr IPv4) -> ConnectionId
  ConnectionIdUnix :: !(SockAddr Unix) -> !(SockAddr Unix) -> ConnectionId

deriving instance Show ConnectionId

instance Eq ConnectionId where
  left == right = connectionIdPair left == connectionIdPair right

instance Ord ConnectionId where
  left `compare` right = connectionIdPair left `compare` connectionIdPair right

makeConnectionId :: SockAddr sockType -> SockAddr sockType -> ConnectionId
makeConnectionId a@(SockAddrIPv6 _ _ _ _) b@(SockAddrIPv6 _ _ _ _) =
  ConnectionIdIPv6 a b
makeConnectionId a@(SockAddrIPv4 _ _) b@(SockAddrIPv4 _ _) =
  ConnectionIdIPv4 a b
makeConnectionId a@(SockAddrUnix _) b@(SockAddrUnix _) =
  ConnectionIdUnix a b

-- | Forget the family information of the addresses.
-- First element is the local address, second is remote.
connectionIdPair :: ConnectionId -> (Socket.SockAddr, Socket.SockAddr)
connectionIdPair connId = case connId of
  ConnectionIdIPv6 x y -> (forgetSockType x, forgetSockType y)
  ConnectionIdIPv4 x y -> (forgetSockType x, forgetSockType y)
  ConnectionIdUnix x y -> (forgetSockType x, forgetSockType y)

data Some (ty :: l -> Type) where
  Some :: ty x -> Some ty

someSockType :: Socket.SockAddr -> Some SockAddr
someSockType sockAddr = case sockAddr of
  Socket.SockAddrInet  pn ha       -> Some (SockAddrIPv4 pn ha)
  Socket.SockAddrInet6 pn fi ha si -> Some (SockAddrIPv6 pn fi ha si)
  Socket.SockAddrUnix  st          -> Some (SockAddrUnix st)

-- | Use a typical `SockAddr` as a type-annotated `SockAddr`, in a continuation
-- which doesn't care about the socket type.
withSockType :: Socket.SockAddr -> (forall sockType . SockAddr sockType -> t) -> t
withSockType addr k = case someSockType addr of
  Some sockAddr -> k sockAddr

-- | Gives `Just` if the second socket address is of the same type as the
-- first one.
matchSockType :: SockAddr sockType -> Socket.SockAddr -> Maybe (SockAddr sockType)
matchSockType (SockAddrIPv6 _ _ _ _) (Socket.SockAddrInet6 pn fi ha si) =
  Just (SockAddrIPv6 pn fi ha si)
matchSockType (SockAddrIPv4 _ _) (Socket.SockAddrInet pn ha) =
  Just (SockAddrIPv4 pn ha)
matchSockType (SockAddrUnix _) (Socket.SockAddrUnix st) =
  Just (SockAddrUnix st)
matchSockType _ _ = Nothing

matchSockAddr :: SockAddr sockType -> Some SockAddr -> Maybe (SockAddr sockType)
matchSockAddr (SockAddrIPv6 _ _ _ _) (Some (SockAddrIPv6 pn fi ha si)) =
  Just (SockAddrIPv6 pn fi ha si)
matchSockAddr (SockAddrIPv4 _ _) (Some (SockAddrIPv4 pn ha)) =
  Just (SockAddrIPv4 pn ha)
matchSockAddr (SockAddrUnix _) (Some (SockAddrUnix st)) =
  Just (SockAddrUnix st)
matchSockAddr _ _ = Nothing

forgetSockType :: SockAddr sockType -> Socket.SockAddr
forgetSockType sockAddr = case sockAddr of
  SockAddrIPv4 pn ha       -> Socket.SockAddrInet  pn ha
  SockAddrIPv6 pn fi ha si -> Socket.SockAddrInet6 pn fi ha si
  SockAddrUnix st          -> Socket.SockAddrUnix  st
