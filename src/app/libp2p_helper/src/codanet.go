package codanet

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"math/rand"
	gonet "net"
	"path"
	"sync"
	"time"

	dsb "github.com/ipfs/go-ds-badger"
	logging "github.com/ipfs/go-log"
	p2p "github.com/libp2p/go-libp2p"
	p2pconnmgr "github.com/libp2p/go-libp2p-connmgr"
	"github.com/libp2p/go-libp2p-core/connmgr"
	"github.com/libp2p/go-libp2p-core/control"
	"github.com/libp2p/go-libp2p-core/crypto"
	"github.com/libp2p/go-libp2p-core/host"
	"github.com/libp2p/go-libp2p-core/metrics"
	"github.com/libp2p/go-libp2p-core/network"
	"github.com/libp2p/go-libp2p-core/peer"
	peerstore "github.com/libp2p/go-libp2p-core/peerstore"
	protocol "github.com/libp2p/go-libp2p-core/protocol"
	"github.com/libp2p/go-libp2p-core/routing"
	discovery "github.com/libp2p/go-libp2p-discovery"
	dht "github.com/libp2p/go-libp2p-kad-dht"
	"github.com/libp2p/go-libp2p-kad-dht/dual"
	"github.com/libp2p/go-libp2p-peerstore/pstoreds"
	pubsub "github.com/libp2p/go-libp2p-pubsub"
	record "github.com/libp2p/go-libp2p-record"
	p2pconfig "github.com/libp2p/go-libp2p/config"
	mdns "github.com/libp2p/go-libp2p/p2p/discovery"
	ma "github.com/multiformats/go-multiaddr"
	manet "github.com/multiformats/go-multiaddr/net"
	"golang.org/x/crypto/blake2b"

	libp2pmplex "github.com/libp2p/go-libp2p-mplex"
	mplex "github.com/libp2p/go-mplex"
)

const NodeStatusTimeout = 10 * time.Second

func parseCIDR(cidr string) gonet.IPNet {
	_, ipnet, err := gonet.ParseCIDR(cidr)
	if err != nil {
		panic(err)
	}
	return *ipnet
}

type getRandomPeersFunc func(num int, from peer.ID) []peer.AddrInfo

const numPxConnectionWorkers int = 8

var (
	logger      = logging.Logger("codanet.Helper")
	NoDHT       bool // option for testing to completely disable the DHT
	WithPrivate bool // option for testing to allow private IPs

	privateCIDRs = []string{
		"10.0.0.0/8",
		"172.16.0.0/12",
		"192.168.0.0/16",
		"100.64.0.0/10",
		"198.18.0.0/15",
		"169.254.0.0/16",
	}

	pxProtocolID         = protocol.ID("/mina/peer-exchange")
	NodeStatusProtocolID = protocol.ID("/mina/node-status")

	privateIpFilter *ma.Filters = nil
)

func initPrivateIpFilter() {
	privateIpFilter = ma.NewFilters()
	if WithPrivate {
		return
	}

	for _, cidr := range privateCIDRs {
		privateIpFilter.AddFilter(parseCIDR(cidr), ma.ActionDeny)
	}
}

func isPrivateAddr(addr ma.Multiaddr) bool {
	return privateIpFilter.AddrBlocked(addr)
}

type CodaConnectionManager struct {
	ctx              context.Context
	host             host.Host
	p2pManager       *p2pconnmgr.BasicConnMgr
	minaPeerExchange bool
	getRandomPeers   getRandomPeersFunc
	OnConnect        func(network.Network, network.Conn)
	OnDisconnect     func(network.Network, network.Conn)
}

func newCodaConnectionManager(maxConnections int, minaPeerExchange bool) *CodaConnectionManager {
	noop := func(net network.Network, c network.Conn) {}

	return &CodaConnectionManager{
		p2pManager:       p2pconnmgr.NewConnManager(25, maxConnections, time.Duration(1*time.Millisecond)),
		OnConnect:        noop,
		OnDisconnect:     noop,
		minaPeerExchange: minaPeerExchange,
	}
}

// proxy connmgr.ConnManager interface to p2pconnmgr.BasicConnMgr
func (cm *CodaConnectionManager) TagPeer(p peer.ID, tag string, weight int) {
	cm.p2pManager.TagPeer(p, tag, weight)
}
func (cm *CodaConnectionManager) UntagPeer(p peer.ID, tag string) { cm.p2pManager.UntagPeer(p, tag) }
func (cm *CodaConnectionManager) UpsertTag(p peer.ID, tag string, upsert func(int) int) {
	cm.p2pManager.UpsertTag(p, tag, upsert)
}
func (cm *CodaConnectionManager) GetTagInfo(p peer.ID) *connmgr.TagInfo {
	return cm.p2pManager.GetTagInfo(p)
}
func (cm *CodaConnectionManager) TrimOpenConns(ctx context.Context) { cm.p2pManager.TrimOpenConns(ctx) }
func (cm *CodaConnectionManager) Protect(p peer.ID, tag string)     { cm.p2pManager.Protect(p, tag) }
func (cm *CodaConnectionManager) Unprotect(p peer.ID, tag string) bool {
	return cm.p2pManager.Unprotect(p, tag)
}
func (cm *CodaConnectionManager) IsProtected(p peer.ID, tag string) bool {
	return cm.p2pManager.IsProtected(p, tag)
}
func (cm *CodaConnectionManager) Close() error { return cm.p2pManager.Close() }

// proxy connmgr.Decayer interface to p2pconnmgr.BasicConnMgr (which implements connmgr.Decayer via struct inheritance)
func (cm *CodaConnectionManager) RegisterDecayingTag(name string, interval time.Duration, decayFn connmgr.DecayFn, bumpFn connmgr.BumpFn) (connmgr.DecayingTag, error) {
	// casting to Decayer here should always succeed
	decayer, _ := interface{}(cm.p2pManager).(connmgr.Decayer)
	tag, err := decayer.RegisterDecayingTag(name, interval, decayFn, bumpFn)
	return tag, err
}

// redirect Notifee() to self for notification interception
func (cm *CodaConnectionManager) Notifee() network.Notifiee { return cm }

// proxy Notifee notifications to p2pconnmgr.BasicConnMgr, intercepting Connected and Disconnected
func (cm *CodaConnectionManager) Listen(net network.Network, addr ma.Multiaddr) {
	cm.p2pManager.Notifee().Listen(net, addr)
}
func (cm *CodaConnectionManager) ListenClose(net network.Network, addr ma.Multiaddr) {
	cm.p2pManager.Notifee().ListenClose(net, addr)
}
func (cm *CodaConnectionManager) OpenedStream(net network.Network, stream network.Stream) {
	cm.p2pManager.Notifee().OpenedStream(net, stream)
}
func (cm *CodaConnectionManager) ClosedStream(net network.Network, stream network.Stream) {
	cm.p2pManager.Notifee().ClosedStream(net, stream)
}
func (cm *CodaConnectionManager) Connected(net network.Network, c network.Conn) {
	logger.Debugf("%s connected to %s", c.LocalPeer(), c.RemotePeer())
	cm.OnConnect(net, c)
	cm.p2pManager.Notifee().Connected(net, c)

	info := cm.GetInfo()
	if len(net.Peers()) <= info.HighWater {
		return
	}

	cm.TrimOpenConns(context.Background())

	logger.Debugf("node=%s disconnecting from peer=%s; max peers=%d peercount=%d", c.LocalPeer(), c.RemotePeer(), info.HighWater, len(net.Peers()))

	if !cm.minaPeerExchange {
		return
	}

	defer func() {
		go func() {
			// small delay to allow for remote peer to read from stream
			time.Sleep(time.Millisecond * 400)
			_ = c.Close()
		}()
	}()

	// select random subset of our peers to send over, then disconnect
	if cm.getRandomPeers == nil {
		logger.Error("getRandomPeers function not set")
		return
	}

	peers := cm.getRandomPeers(info.LowWater, c.RemotePeer())
	bz, err := json.Marshal(peers)
	if err != nil {
		logger.Error("failed to marshal peers", err)
		return
	}

	stream, err := cm.host.NewStream(cm.ctx, c.RemotePeer(), pxProtocolID)
	if err != nil {
		logger.Debug("failed to open stream", err)
		return
	}

	n, err := stream.Write(bz)
	if err != nil {
		logger.Debug("failed to write to stream", err)
		return
	} else if n != len(bz) {
		logger.Debug("failed to write all data to stream")
		return
	}

	logger.Debugf("wrote peers to stream %s", stream.Protocol())
}

func (cm *CodaConnectionManager) Disconnected(net network.Network, c network.Conn) {
	cm.OnDisconnect(net, c)
	cm.p2pManager.Notifee().Disconnected(net, c)
}

// proxy remaining p2pconnmgr.BasicConnMgr methods for access
func (cm *CodaConnectionManager) GetInfo() p2pconnmgr.CMInfo {
	return cm.p2pManager.GetInfo()
}

// Helper contains all the daemon state
type Helper struct {
	Host              host.Host
	Mdns              *mdns.Service
	Dht               *dual.DHT
	Ctx               context.Context
	Pubsub            *pubsub.PubSub
	Logger            logging.EventLogger
	Rendezvous        string
	Discovery         *discovery.RoutingDiscovery
	Me                peer.ID
	GatingState       *CodaGatingState
	ConnectionManager *CodaConnectionManager
	BandwidthCounter  *metrics.BandwidthCounter
	MsgStats          *MessageStats
	Seeds             []peer.AddrInfo
	NodeStatus        string
	pxDiscoveries     chan peer.AddrInfo
}

type MessageStats struct {
	min   uint64
	avg   uint64
	max   uint64
	total uint64
	sync.RWMutex
}

func (ms *MessageStats) UpdateMetrics(val uint64) {
	ms.Lock()
	defer ms.Unlock()
	if ms.max < val {
		ms.max = val
	}

	if ms.min > val {
		ms.min = val
	}

	ms.total++
	if ms.avg == 0 {
		ms.avg = val
	} else {
		ms.avg = (ms.avg*(ms.total-1) + val) / ms.total
	}
}

type safeStats struct {
	Min float64
	Max float64
	Avg float64
}

func (ms *MessageStats) GetStats() *safeStats {
	ms.RLock()
	defer ms.RUnlock()

	return &safeStats{
		Min: float64(ms.min),
		Max: float64(ms.max),
		Avg: float64(ms.avg),
	}
}

// this type implements the ConnectionGating interface
// https://godoc.org/github.com/libp2p/go-libp2p-core/connmgr#ConnectionGating
// the comments of the functions below are taken from those docs.
type CodaGatingState struct {
	logger                  logging.EventLogger
	KnownPrivateAddrFilters *ma.Filters
	BannedAddrFilters       *ma.Filters
	TrustedAddrFilters      *ma.Filters
	BannedPeers             *peer.Set
	TrustedPeers            *peer.Set
}

// NewCodaGatingState returns a new CodaGatingState
func NewCodaGatingState(bannedAddrFilters *ma.Filters, trustedAddrFilters *ma.Filters, bannedPeers *peer.Set, trustedPeers *peer.Set) *CodaGatingState {
	logger := logging.Logger("codanet.CodaGatingState")

	if bannedAddrFilters == nil {
		bannedAddrFilters = ma.NewFilters()
	}

	if trustedAddrFilters == nil {
		trustedAddrFilters = ma.NewFilters()
	}

	if bannedPeers == nil {
		bannedPeers = peer.NewSet()
	}

	if trustedPeers == nil {
		trustedPeers = peer.NewSet()
	}

	// we initialize the known private addr filters to reject all ip addresses initially
	knownPrivateAddrFilters := ma.NewFilters()
	knownPrivateAddrFilters.AddFilter(parseCIDR("0.0.0.0/0"), ma.ActionDeny)

	return &CodaGatingState{
		logger:                  logger,
		BannedAddrFilters:       bannedAddrFilters,
		TrustedAddrFilters:      trustedAddrFilters,
		KnownPrivateAddrFilters: knownPrivateAddrFilters,
		BannedPeers:             bannedPeers,
		TrustedPeers:            trustedPeers,
	}
}

func (gs *CodaGatingState) MarkPrivateAddrAsKnown(addr ma.Multiaddr) {
	if isPrivateAddr(addr) && gs.KnownPrivateAddrFilters.AddrBlocked(addr) {
		gs.logger.Infof("marking private addr %v as known", addr)

		ip, err := manet.ToIP(addr)
		if err != nil {
			panic(err)
		}

		bits := len(ip) * 8
		ipNet := gonet.IPNet{
			IP:   ip,
			Mask: gonet.CIDRMask(bits, bits),
		}
		gs.KnownPrivateAddrFilters.AddFilter(ipNet, ma.ActionAccept)
	}
}

func (gs *CodaGatingState) isPeerTrusted(p peer.ID) bool {
	return gs.TrustedPeers.Contains(p)
}

func (gs *CodaGatingState) isPeerBanned(p peer.ID) bool {
	return gs.BannedPeers.Contains(p)
}

// checks if a peer id is allowed to dial/accept
func (gs *CodaGatingState) isAllowedPeer(p peer.ID) bool {
	return gs.isPeerTrusted(p) || !gs.isPeerBanned(p)
}

func (gs *CodaGatingState) isAddrTrusted(addr ma.Multiaddr) bool {
	return !gs.TrustedAddrFilters.AddrBlocked(addr)
}

func (gs *CodaGatingState) isAddrBanned(addr ma.Multiaddr) bool {
	return gs.BannedAddrFilters.AddrBlocked(addr)
}

// checks if an address is allowed to dial/accept
func (gs *CodaGatingState) isAllowedAddr(addr ma.Multiaddr) bool {
	publicOrKnownPrivate := !isPrivateAddr(addr) || !gs.KnownPrivateAddrFilters.AddrBlocked(addr)
	return gs.isAddrTrusted(addr) || (!gs.isAddrBanned(addr) && publicOrKnownPrivate)
}

// checks if a peer is allowed to dial/accept; if the peer is in the trustlist, the address checks are overriden
func (gs *CodaGatingState) isAllowedPeerWithAddr(p peer.ID, addr ma.Multiaddr) bool {
	return gs.isPeerTrusted(p) || (gs.isAllowedPeer(p) && gs.isAllowedAddr(addr))
}

func (gs *CodaGatingState) logGate() {
	gs.logger.Debugf("gated a connection with config: %+v", gs)
}

// InterceptPeerDial tests whether we're permitted to Dial the specified peer.
//
// This is called by the network.Network implementation when dialling a peer.
func (gs *CodaGatingState) InterceptPeerDial(p peer.ID) (allow bool) {
	allow = gs.isAllowedPeer(p)

	if !allow {
		gs.logger.Infof("disallowing peer dial to: %v (peer)", p)
		gs.logGate()
	}

	return
}

// InterceptAddrDial tests whether we're permitted to dial the specified
// multiaddr for the given peer.
//
// This is called by the network.Network implementation after it has
// resolved the peer's addrs, and prior to dialling each.
func (gs *CodaGatingState) InterceptAddrDial(id peer.ID, addr ma.Multiaddr) (allow bool) {
	allow = gs.isAllowedPeerWithAddr(id, addr)

	if !allow {
		gs.logger.Infof("disallowing peer dial to: %v + %v (peer + address)", id, addr)
		gs.logGate()
	}

	return
}

// InterceptAccept tests whether an incipient inbound connection is allowed.
//
// This is called by the upgrader, or by the transport directly (e.g. QUIC,
// Bluetooth), straight after it has accepted a connection from its socket.
func (gs *CodaGatingState) InterceptAccept(addrs network.ConnMultiaddrs) (allow bool) {
	remoteAddr := addrs.RemoteMultiaddr()
	allow = gs.isAddrTrusted(remoteAddr) || !gs.isAddrBanned(remoteAddr)

	if !allow {
		gs.logger.Infof("refusing to accept inbound connection from addr: %v", remoteAddr)
		gs.logGate()
	}

	// If we are receiving a connection, and the remote address is private,
	// then we infer that we should be able to connect to that private address.
	if allow {
		gs.MarkPrivateAddrAsKnown(remoteAddr)
	}

	return
}

// InterceptSecured tests whether a given connection, now authenticated,
// is allowed.
//
// This is called by the upgrader, after it has performed the security
// handshake, and before it negotiates the muxer, or by the directly by the
// transport, at the exact same checkpoint.
func (gs *CodaGatingState) InterceptSecured(_ network.Direction, id peer.ID, addrs network.ConnMultiaddrs) (allow bool) {
	// note: we don't care about the direction (inbound/outbound). all
	// connections in coda are symmetric: if i am allowed to connect to
	// you, you are allowed to connect to me.
	remoteAddr := addrs.RemoteMultiaddr()
	allow = gs.isAllowedPeerWithAddr(id, remoteAddr)

	if !allow {
		gs.logger.Infof("refusing to accept inbound connection from authenticated addr: %v", remoteAddr)
		gs.logGate()
	}

	return
}

// InterceptUpgraded tests whether a fully capable connection is allowed.
//
// At this point, the connection a multiplexer has been selected.
// When rejecting a connection, the gater can return a DisconnectReason.
// Refer to the godoc on the ConnectionGating type for more information.
//
// NOTE: the go-libp2p implementation currently IGNORES the disconnect reason.
func (gs *CodaGatingState) InterceptUpgraded(network.Conn) (allow bool, reason control.DisconnectReason) {
	allow = true
	reason = control.DisconnectReason(0)
	return
}

func (h *Helper) getRandomPeers(num int, from peer.ID) []peer.AddrInfo {
	peers := h.Host.Peerstore().Peers()
	if len(peers)-2 < num {
		num = len(peers) - 2 // -2 because excluding ourself and the peer we are sending this to
	}

	ret := make([]peer.AddrInfo, num)
	rand.Shuffle(len(peers), func(i, j int) { peers[i], peers[j] = peers[j], peers[i] })
	idx := 0

	for i := 0; i < num; i++ {
		for {
			if idx >= len(peers) {
				return ret
			} else if peers[idx] != h.Host.ID() && peers[idx] != from && len(h.Host.Peerstore().PeerInfo(peers[idx]).Addrs) != 0 {
				break
			} else {
				idx += 1
			}
		}

		ret[i] = h.Host.Peerstore().PeerInfo(peers[idx])
		idx += 1
	}

	logger.Debugf("node=%s sending random peers", h.Host.ID(), ret)
	return ret
}

type customValidator struct {
	Base record.Validator
}

func (cv customValidator) Validate(key string, value []byte) error {
	logger.Debugf("DHT Validating: %s = %s", key, value)
	return cv.Base.Validate(key, value)
}

func (cv customValidator) Select(key string, values [][]byte) (int, error) {
	logger.Debugf("DHT Selecting Among: %s = %s", key, bytes.Join(values, []byte("; ")))
	return cv.Base.Select(key, values)
}

func (h *Helper) handlePxStreams(s network.Stream) {
	defer func() {
		_ = s.Close()
	}()

	stat := s.Conn().Stat()
	if stat.Direction != network.DirOutbound {
		return
	}

	connInfo := h.ConnectionManager.GetInfo()
	if connInfo.ConnCount >= connInfo.LowWater {
		return
	}

	buf := make([]byte, 8192)
	_, err := s.Read(buf)
	if err != nil && err != io.EOF {
		logger.Debugf("failed to decode list of peers err=%s", err)
		return
	}

	r := bytes.NewReader(buf)
	peers := []peer.AddrInfo{}
	dec := json.NewDecoder(r)
	err = dec.Decode(&peers)
	if err != nil {
		logger.Debugf("failed to decode list of peers err=%s", err)
		return
	}

	for _, p := range peers {
		select {
		case h.pxDiscoveries <- p:
		default:
			logger.Debugf("peer discoveries channel full; dropping peer %v", p)
		}
	}
}

func (h *Helper) handleNodeStatusStreams(s network.Stream) {
	defer func() {
		err := s.Close()
		if err != nil {
			logger.Error("failed to close write side of stream", err)
			return
		}

		<-time.After(NodeStatusTimeout)

		err = s.Reset()
		if err != nil {
			logger.Error("failed to reset stream", err)
		}
	}()

	n, err := s.Write([]byte(h.NodeStatus))
	if err != nil {
		logger.Error("failed to write to stream", err)
		return
	} else if n != len(h.NodeStatus) {
		logger.Error("failed to write all data to stream")
		return
	}

	logger.Debugf("wrote node status to stream %s", s.Protocol())
}

func (h Helper) pxConnectionWorker() {
	for peer := range h.pxDiscoveries {
		connInfo := h.ConnectionManager.GetInfo()
		if connInfo.ConnCount < connInfo.LowWater {
			err := h.Host.Connect(h.Ctx, peer)
			if err != nil {
				logger.Debugf("failed to connect to peer %v err=%s", peer, err)
			} else {
				logger.Debugf("connected to peer! %v", peer)
			}
		} else {
			logger.Debugf("discovered peer (%v) via peer exchange, but already have too many connections; adding to peerstore, but refusing to connect", peer)
			h.Host.Peerstore().AddAddrs(peer.ID, peer.Addrs, peerstore.ConnectedAddrTTL)
		}
	}
}

// MakeHelper does all the initialization to run one host
func MakeHelper(ctx context.Context, listenOn []ma.Multiaddr, externalAddr ma.Multiaddr, statedir string, pk crypto.PrivKey, networkID string, seeds []peer.AddrInfo, gatingState *CodaGatingState, maxConnections int, minaPeerExchange bool) (*Helper, error) {
	me, err := peer.IDFromPrivateKey(pk)
	if err != nil {
		return nil, err
	}

	initPrivateIpFilter()

	dso := dsb.DefaultOptions

	ds, err := dsb.NewDatastore(path.Join(statedir, "libp2p-peerstore-v0"), &dso)
	if err != nil {
		return nil, err
	}

	dsoDht := dsb.DefaultOptions
	dsDht, err := dsb.NewDatastore(path.Join(statedir, "libp2p-dht-v0"), &dsoDht)
	if err != nil {
		return nil, err
	}

	ps, err := pstoreds.NewPeerstore(ctx, ds, pstoreds.DefaultOpts())
	if err != nil {
		return nil, err
	}

	rendezvousString := fmt.Sprintf("/coda/0.0.1/%s", networkID)

	pnetKey := blake2b.Sum256([]byte(rendezvousString))

	// custom validator to omit the ipns validation.
	rv := customValidator{Base: record.NamespacedValidator{"pk": record.PublicKeyValidator{}}}

	var kad *dual.DHT

	mplex.MaxMessageSize = 1 << 30

	connManager := newCodaConnectionManager(maxConnections, minaPeerExchange)
	bandwidthCounter := metrics.NewBandwidthCounter()

	host, err := p2p.New(ctx,
		p2p.Muxer("/coda/mplex/1.0.0", libp2pmplex.DefaultTransport),
		p2p.Identity(pk),
		p2p.Peerstore(ps),
		p2p.DisableRelay(),
		p2p.ConnectionGater(gatingState),
		p2p.ConnectionManager(connManager),
		p2p.ListenAddrs(listenOn...),
		p2p.AddrsFactory(func(as []ma.Multiaddr) []ma.Multiaddr {
			if externalAddr != nil {
				as = append(as, externalAddr)
			}

			return as
		}),
		p2p.NATPortMap(),
		p2p.Routing(
			p2pconfig.RoutingC(func(host host.Host) (routing.PeerRouting, error) {
				if NoDHT {
					return nil, nil
				}

				kad, err = dual.New(ctx, host,
					dual.WanDHTOption(dht.Datastore(dsDht)),
					dual.DHTOption(dht.Validator(rv)),
					dual.DHTOption(dht.BootstrapPeers(seeds...)),
					dual.DHTOption(dht.ProtocolPrefix("/coda")),
				)
				return kad, err
			})),
		p2p.UserAgent("github.com/codaprotocol/coda/tree/master/src/app/libp2p_helper"),
		p2p.PrivateNetwork(pnetKey[:]),
		p2p.BandwidthReporter(bandwidthCounter),
	)

	if err != nil {
		return nil, err
	}

	// nil fields are initialized by beginAdvertising
	h := &Helper{
		Host:              host,
		Ctx:               ctx,
		Mdns:              nil,
		Dht:               kad,
		Pubsub:            nil,
		Logger:            logger,
		Rendezvous:        rendezvousString,
		Discovery:         nil,
		Me:                me,
		GatingState:       gatingState,
		ConnectionManager: connManager,
		BandwidthCounter:  bandwidthCounter,
		MsgStats:          &MessageStats{min: math.MaxUint64},
		Seeds:             seeds,
		pxDiscoveries:     nil,
	}

	if !minaPeerExchange {
		return h, nil
	}

	h.pxDiscoveries = make(chan peer.AddrInfo, maxConnections*3)
	for w := 0; w < numPxConnectionWorkers; w++ {
		go h.pxConnectionWorker()
	}

	connManager.getRandomPeers = h.getRandomPeers
	connManager.ctx = ctx
	connManager.host = host
	h.Host.SetStreamHandler(pxProtocolID, h.handlePxStreams)
	h.Host.SetStreamHandler(NodeStatusProtocolID, h.handleNodeStatusStreams)
	return h, nil
}
