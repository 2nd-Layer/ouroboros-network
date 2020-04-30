{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections  #-}

module Ouroboros.Network.Diffusion
  ( DiffusionTracers (..)
  , DiffusionArguments (..)
  , AcceptedConnectionsLimit (..)
  , DiffusionApplications (..)
  , OuroborosApplication (..)
  , runDataDiffusion
    -- * re-exports
  , simpleSingletonVersions
  , IPSubscriptionTarget (..)
  , DnsSubscriptionTarget (..)
  , ConnectionId' (..)
  , ConnectionId
  , MaybeAddress (..)
  , WithConnectionId' (..)
  , WithConnectionId
  )
  where

import qualified Control.Concurrent.Async as Async
import           Control.Exception (IOException, SomeException, fromException)
import           Control.Tracer (Tracer, traceWith, nullTracer)
import           Data.Functor (void)
import           Data.Functor.Contravariant (contramap)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)

import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import           Network.Socket (AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Connections.Trace
import           Ouroboros.Network.Connections.Types (Connections,
                   Provenance (..))
import           Ouroboros.Network.Connections.Concurrent as Concurrent
import           Ouroboros.Network.Connections.Socket.Client (Bind (..))
import           Ouroboros.Network.Connections.Socket.Server as Server (acceptLoop, withSocket)
import           Ouroboros.Network.Snocket (LocalAddress, SocketSnocket, LocalSnocket, LocalFD)
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.Protocol.Handshake.Version

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..) )
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode ( NodeToNodeVersion (..)
                                              , AcceptedConnectionsLimit (..)
                                              , AcceptConnectionsPolicyTrace (..)
                                              )
import qualified Ouroboros.Network.NodeToNode   as NodeToNode
import           Ouroboros.Network.Socket ( ConnectionHandle
                                          , NetworkServerTracers (..)
                                          , NetworkConnectTracers (..)
                                          , SomeResponderApplication (..)
                                          , SomeVersionedApplication (..)
                                          )
import qualified Ouroboros.Network.Subscription as Subscription (worker)
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Dns

data DiffusionTracers = DiffusionTracers {
      dtIpSubscriptionTracer   :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
       -- ^ IP subscription tracer
    , dtDnsSubscriptionTracer  :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
      -- ^ DNS subscription tracer
    , dtDnsResolverTracer      :: Tracer IO (WithDomainName DnsTrace)
      -- ^ DNS resolver tracer
    , dtMuxTracer              :: Tracer IO (WithMuxBearer (ConnectionId Socket.SockAddr) MuxTrace)
      -- ^ Mux tracer
    , dtMuxLocalTracer         :: Tracer IO (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)
      -- ^ Mux tracer for local clients
    , dtHandshakeTracer        :: Tracer IO NodeToNode.HandshakeTr
      -- ^ Handshake protocol tracer
    , dtHandshakeLocalTracer   :: Tracer IO NodeToClient.HandshakeTr
      -- ^ Handshake protocol tracer for local clients
    , dtConnectionTracer       :: Tracer IO (WithConnectionId'
                                             Socket.SockAddr
                                             (MaybeAddress Socket.SockAddr)
                                             ConnectionTrace)
    , dtLocalConnectionTracer  :: Tracer IO (WithConnectionId'
                                             LocalAddress
                                             (MaybeAddress LocalAddress)
                                             ConnectionTrace)
    , dtErrorPolicyTracer      :: Tracer IO (WithAddress Socket.SockAddr ErrorPolicyTrace)
    , dtLocalErrorPolicyTracer :: Tracer IO (WithAddress LocalAddress    ErrorPolicyTrace)
    , dtAcceptPolicyTracer     :: Tracer IO AcceptConnectionsPolicyTrace
      -- ^ Trace rate limiting of accepted connections
    }


-- | Network Node argumets
--
data DiffusionArguments = DiffusionArguments {
      daAddresses    :: [AddrInfo]
      -- ^ diffusion addresses
    , daLocalAddress :: FilePath
      -- ^ address for local clients
    , daIpProducers  :: IPSubscriptionTarget
      -- ^ ip subscription addresses
    , daDnsProducers :: [DnsSubscriptionTarget]
      -- ^ list of domain names to subscribe to
    , daAcceptedConnectionsLimit :: AcceptedConnectionsLimit
      -- ^ parameters for limiting number of accepted connections
    }

data DiffusionApplications = DiffusionApplications {

      daResponderApplication      :: Versions
                                       NodeToNodeVersion
                                       DictVersion
                                       (ConnectionId Socket.SockAddr ->
                                          OuroborosApplication
                                            ResponderApp
                                            ByteString IO Void ())
      -- ^ NodeToNode reposnder application (server role)

    , daInitiatorApplication      :: Versions
                                       NodeToNodeVersion
                                       DictVersion 
                                       (ConnectionId Socket.SockAddr ->
                                          OuroborosApplication
                                            InitiatorApp
                                            ByteString IO () Void)
      -- ^ NodeToNode initiator application (client role)

    , daLocalResponderApplication :: Versions
                                       NodeToClientVersion
                                       DictVersion
                                       (ConnectionId LocalAddress ->
                                          OuroborosApplication
                                            ResponderApp
                                            ByteString IO Void ())
      -- ^ NodeToClient responder applicaton (server role)

    , daErrorPolicies :: ErrorPolicies
      -- ^ error policies
    }

-- | The local server only accepts incoming requests (from clients in the
-- node-to-client protocol.)
data LocalRequest (p :: Provenance) where
  ClientConnection :: LocalRequest Remote

-- | The node-to-node server admits locally- and remotely-initiated connections.
-- Remotely come from the server accept loops. Locally come from IP and DNS
-- subscription systems.
data Request (p :: Provenance) where
  PeerConnection            :: Request Remote
  IpSubscriptionConnection  :: Request Local
  DnsSubscriptionConnection :: Request Local

runDataDiffusion
    :: DiffusionTracers
    -> DiffusionArguments 
    -> DiffusionApplications
    -> IO ()
runDataDiffusion tracers
                 DiffusionArguments { daAddresses
                                    , daLocalAddress
                                    , daIpProducers
                                    , daDnsProducers
                                    , daAcceptedConnectionsLimit
                                    }
                 applications@DiffusionApplications { daErrorPolicies } =
    withIOManager $ \iocp -> do

    let -- snocket for remote communication.
        snocket :: SocketSnocket
        snocket = Snocket.socketSnocket iocp

        -- snocket for local clients connected using Unix socket or named pipe.
        -- we currently don't support remotely connected local clients.  If we
        -- need to we can add another adress for local clients.
        localSnocket :: LocalSnocket
        localSnocket = Snocket.localSnocket iocp daLocalAddress

        -- Define how to fulfill node-to-client and node-to-node connection
        -- requests. The GADTs `LocalRequest` and `Request` describe all of the
        -- possibilities. In particular, the node-to-client protocol is restricted
        -- to responder only, i.e. this node will never try to initiate a
        -- node-to-client protocol.
        localConnectionRequest
          :: LocalRequest provenance
          -> SomeVersionedApplication
              NodeToClientVersion
              DictVersion LocalAddress provenance
        localConnectionRequest ClientConnection =
          SomeVersionedResponderApp
            (NetworkServerTracers
              dtMuxLocalTracer
              dtHandshakeLocalTracer
              dtLocalConnectionTracer
              dtAcceptPolicyTracer)
            ((fmap . fmap) SomeResponderApplication (daLocalResponderApplication applications))

        -- Note-to-node connections request: for a PeerConnection we do the
        -- responder app `daResponderApplication`, and for the others we choose
        -- the `daInitiatorApplication`. The types leave little room for error.
        connectionRequest
          :: Request provenance
          -> SomeVersionedApplication NodeToNodeVersion DictVersion Socket.SockAddr provenance
        connectionRequest PeerConnection = SomeVersionedResponderApp
          (NetworkServerTracers
            dtMuxTracer
            dtHandshakeTracer
            dtConnectionTracer
            dtAcceptPolicyTracer)
          ((fmap . fmap) SomeResponderApplication (daResponderApplication applications))
        -- IP or DNS subscription requests are locally-initiated (requests
        -- from subscribers to _us_ are `PeerConnection` above.
        connectionRequest IpSubscriptionConnection  = SomeVersionedInitiatorApp
          (NetworkConnectTracers
            dtMuxTracer
            dtHandshakeTracer
            (withKnownRemoteAddress `contramap` dtConnectionTracer))
          (daInitiatorApplication applications)
        connectionRequest DnsSubscriptionConnection = SomeVersionedInitiatorApp
          (NetworkConnectTracers
            dtMuxTracer
            dtHandshakeTracer
            (withKnownRemoteAddress `contramap` dtConnectionTracer))
          (daInitiatorApplication applications)

        localAcceptException :: LocalAddress -> SomeException -> IO ()
        localAcceptException a e = case fromException e of
          Just (e' :: IOException) ->
            traceWith
              dtLocalConnectionTracer
              (WithConnectionId
                (ConnectionId a UnknownAddress)
                (ConnectionTraceAcceptException e'))
          Nothing -> pure ()

        acceptException :: Socket.SockAddr -> SomeException -> IO ()
        acceptException a e = case fromException e of
          Just (e' :: IOException) ->
            traceWith
              dtConnectionTracer
              (WithConnectionId
                (ConnectionId a UnknownAddress)
                (ConnectionTraceAcceptException e'))
          Nothing -> pure ()

        -- How to run a local server: take the `daLocalAddress` and run an
        -- accept loop on it against the node-to-client connetions.
        runLocalServer
          :: Connections (ConnectionId LocalAddress) LocalFD LocalRequest accept reject IO
          -> IO Void
        runLocalServer n2cConnections =
          Server.withSocket localSnocket addr $ \boundAddr socket ->
            Server.acceptLoop
              localSnocket
              -- we are not doing any rate limiting, so no reason for using
              -- a tracer
              nullTracer
              (AcceptedConnectionsLimit maxBound maxBound 0)
              n2cConnections
              boundAddr
              ClientConnection
              (localAcceptException boundAddr)
              (Snocket.accept localSnocket socket)
          where
            addr = Snocket.localAddressFromPath daLocalAddress

        -- A node-to-node server will be run against a common connections
        -- manager but on potentially many different bind addresses.
        -- This function will be mapped over the `daAddresses`.
        runServer
          :: Connections (ConnectionId Socket.SockAddr) Socket.Socket Request accept reject IO
          -> AddrInfo
          -> IO Void
        runServer n2nConnections addrInfo =
          Server.withSocket snocket addr $ \boundAddr socket ->
            Server.acceptLoop
              snocket
              dtAcceptPolicyTracer
              daAcceptedConnectionsLimit
              n2nConnections
              boundAddr
              PeerConnection
              (acceptException boundAddr)
              (Snocket.accept snocket socket)
          where
            addr = Socket.addrAddress addrInfo

    -- Get 2 connection managers: one for node-to-client (n2c) and one for
    -- node-to-node (n2n).
    --
    -- These connections managers will throwTo any exceptions thrown by their
    -- connection handlers (whether incoming or outgoing) to this thread,
    -- wrapped in a special type `ExceptionInHandler`. Since we don't catch
    -- these, an exception in a handler will therefore bring down the whole
    -- node. This is apparently what we want, and `ErrorPolicies` is used to
    -- determine precisely when that should happen.
    NodeToClient.withConnections localErrorPolicy localSnocket localConnectionRequest $ \n2cConnections ->
      NodeToNode.withConnections errorPolicy      snocket      connectionRequest      $ \n2nConnections -> do
        -- Run every thread concurrently such that an exception from any of
        -- them will kill the others and be re-thrown here.
        -- The application terminates when they are all done.
        let threads :: [Async.Concurrently ()]
            threads = fmap Async.Concurrently $ mconcat
              [ [ void (runLocalServer n2cConnections) ]
              , ( void . runServer n2nConnections) <$> daAddresses
              , [ void (runIpSubscriptionWorker snocket n2nConnections) ]
              , ( void . runDnsSubscriptionWorker snocket n2nConnections) <$> daDnsProducers
              ]
        -- mconcat'ing the `Concurrently ()`s runs them concurrently.
        _ <- Async.runConcurrently (mconcat threads)
        pure ()

  where
    DiffusionTracers { dtIpSubscriptionTracer
                     , dtDnsSubscriptionTracer
                     , dtDnsResolverTracer
                     , dtMuxTracer
                     , dtMuxLocalTracer
                     , dtHandshakeTracer
                     , dtHandshakeLocalTracer
                     , dtConnectionTracer
                     , dtLocalConnectionTracer
                     -- , dtErrorPolicyTracer
                     -- , dtLocalErrorPolicyTracer
                     , dtAcceptPolicyTracer
                     } = tracers

    initiatorLocalAddresses :: LocalAddresses
    initiatorLocalAddresses = LocalAddresses
      { laIpv4 =
          -- IPv4 address
          --
          -- We can't share portnumber with our server since we run separate
          -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
          -- applications instead of a 'MuxInitiatorAndResponderApplication'.
          -- This means we don't utilise full duplex connection.
          if any (\ai -> Socket.addrFamily ai == Socket.AF_INET) daAddresses
            then Just (Socket.SockAddrInet 0 0)
            else Nothing
      , laIpv6 =
          -- IPv6 address
          if any (\ai -> Socket.addrFamily ai == Socket.AF_INET6) daAddresses
            then Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 0) 0)
            else Nothing
      , laUnix = Nothing
      }

    errorPolicy, localErrorPolicy :: ErrorPolicies
    errorPolicy = NodeToNode.remoteNetworkErrorPolicy <> daErrorPolicies
    localErrorPolicy  = NodeToNode.localNetworkErrorPolicy <> daErrorPolicies

    runIpSubscriptionWorker
      :: SocketSnocket
      -> Connections (ConnectionId Socket.SockAddr) Socket.Socket Request (Concurrent.Reject reject) (Concurrent.Accept (ConnectionHandle IO))  IO
      -> IO ()
    runIpSubscriptionWorker sn connections =
      case ipSubscriptionTargets (ispIps daIpProducers) initiatorLocalAddresses of
        -- There were no addresses in the config (ispIps) which have an
        -- address in initiatorLocalAddresses of the same family.
        --
        -- TODO: add a log warning message.  This could be a misconfiguration.
        Nothing -> pure ()
        Just connIds -> Subscription.worker
          (contramap (WithIPList initiatorLocalAddresses (ispIps daIpProducers)) dtIpSubscriptionTracer)
          (withKnownRemoteAddress `contramap` dtConnectionTracer)
          errorPolicy
          ((,Bind) <$> connIds)
          (ispValency daIpProducers)
          ipRetryDelay
          sn
          connections
          IpSubscriptionConnection

    -- FIXME probably not ideal that we make a resolver `mkResolverIO` for
    -- each DNS target. Should make one (not have it take a port) and use it
    -- for all of the tagets.
    runDnsSubscriptionWorker
      :: SocketSnocket
      -> Connections (ConnectionId Socket.SockAddr) Socket.Socket Request (Concurrent.Reject reject) (Concurrent.Accept (ConnectionHandle IO))  IO
      -> DnsSubscriptionTarget
      -> IO ()
    runDnsSubscriptionWorker sn connections target = mkResolverIO (dstPort target) $ \resolver -> do
      addrs <- dnsResolve
        (contramap (WithDomainName (dstDomain target)) dtDnsResolverTracer)
        resolver
        (dstDomain target)
      case ipSubscriptionTargets addrs initiatorLocalAddresses of
        -- TODO: add a log warning message.  This could be a misconfiguration.
        Nothing -> pure ()
        Just connIds -> Subscription.worker
          (WithDomainName (dstDomain target) `contramap` dtDnsSubscriptionTracer)
          (withKnownRemoteAddress `contramap` dtConnectionTracer)
          errorPolicy
          ((,Bind) <$> connIds)
          (dstValency target)
          ipRetryDelay
          sn
          connections
          DnsSubscriptionConnection
