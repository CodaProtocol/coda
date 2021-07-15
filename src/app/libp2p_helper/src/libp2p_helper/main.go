package main

import (
	"bufio"
	"codanet"
	"context"
	cryptorand "crypto/rand"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"log"
	gonet "net"
	"net/http"
	"os"
	"runtime/debug"
	"strconv"
	"sync"
	"time"

	"github.com/go-errors/errors"
	logging "github.com/ipfs/go-log/v2"
	crypto "github.com/libp2p/go-libp2p-core/crypto"
	net "github.com/libp2p/go-libp2p-core/network"
	peer "github.com/libp2p/go-libp2p-core/peer"
	peerstore "github.com/libp2p/go-libp2p-core/peerstore"
	protocol "github.com/libp2p/go-libp2p-core/protocol"
	discovery "github.com/libp2p/go-libp2p-discovery"
	pubsub "github.com/libp2p/go-libp2p-pubsub"
	mdns "github.com/libp2p/go-libp2p/p2p/discovery"
	"github.com/multiformats/go-multiaddr"
	ma "github.com/multiformats/go-multiaddr"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

type subscription struct {
	Sub    *pubsub.Subscription
	Idx    int
	Ctx    context.Context
	Cancel context.CancelFunc
}

type validationStatus struct {
	Completion chan string
	TimedOutAt *time.Time
}

type app struct {
	P2p             *codanet.Helper
	Ctx             context.Context
	Subs            map[int]subscription
	Topics          map[string]*pubsub.Topic
	Validators      map[int]*validationStatus
	ValidatorMutex  *sync.Mutex
	Streams         map[int]net.Stream
	StreamsMutex    sync.Mutex
	Out             *bufio.Writer
	OutChan         chan interface{}
	Bootstrapper    io.Closer
	AddedPeers      []peer.AddrInfo
	UnsafeNoTrustIP bool

	// development configuration options
	NoMDNS    bool
	NoDHT     bool
	NoUpcalls bool
}

var seqs = make(chan int)

type methodIdx int

const (
	// when editing this block, see the README for how to update methodidx_jsonenum
	configure methodIdx = iota
	listen
	publish
	subscribe
	unsubscribe
	validationComplete
	generateKeypair
	openStream
	closeStream
	resetStream
	sendStreamMsg
	removeStreamHandler
	addStreamHandler
	listeningAddrs
	addPeer
	beginAdvertising
	findPeer
	listPeers
	setGatingConfig
	setNodeStatus
	getPeerNodeStatus
)

const validationTimeout = 5 * time.Minute

type codaPeerInfo struct {
	Libp2pPort int    `json:"libp2p_port"`
	Host       string `json:"host"`
	PeerID     string `json:"peer_id"`
}

type envelope struct {
	Method methodIdx   `json:"method"`
	Seqno  int         `json:"seqno"`
	Body   interface{} `json:"body"`
}

func (app *app) writeMsg(msg interface{}) {
	if app.NoUpcalls {
		return
	}

	app.OutChan <- msg
}

type action interface {
	run(app *app) (interface{}, error)
}

// TODO: wrap these in a new type, encode them differently in the rpc mainloop

type wrappedError struct {
	e   error
	tag string
}

func (w wrappedError) Error() string {
	return fmt.Sprintf("%s error: %s", w.tag, w.e.Error())
}

func (w wrappedError) Unwrap() error {
	return w.e
}

func wrapError(e error, tag string) error { return wrappedError{e: e, tag: tag} }

func badRPC(e error) error {
	return wrapError(e, "internal RPC error")
}

func badp2p(e error) error {
	return wrapError(e, "libp2p error")
}

func badHelper(e error) error {
	return wrapError(e, "initializing helper")
}

func badAddr(e error) error {
	return wrapError(e, "initializing external addr")
}

func needsConfigure() error {
	return badRPC(errors.New("helper not yet configured"))
}

func needsDHT() error {
	return badRPC(errors.New("helper not yet joined to pubsub"))
}

func parseMultiaddrWithID(ma multiaddr.Multiaddr, id peer.ID) (*codaPeerInfo, error) {
	ipComponent, tcpMaddr := multiaddr.SplitFirst(ma)
	if !(ipComponent.Protocol().Code == multiaddr.P_IP4 || ipComponent.Protocol().Code == multiaddr.P_IP6) {
		return nil, badRPC(errors.New(fmt.Sprintf("only IP connections are supported right now, how did this peer connect?: %s", ma.String())))
	}

	tcpComponent, _ := multiaddr.SplitFirst(tcpMaddr)
	if tcpComponent.Protocol().Code != multiaddr.P_TCP {
		return nil, badRPC(errors.New("only TCP connections are supported right now, how did this peer connect?"))
	}

	port, err := strconv.Atoi(tcpComponent.Value())
	if err != nil {
		return nil, err
	}

	return &codaPeerInfo{Libp2pPort: port, Host: ipComponent.Value(), PeerID: peer.Encode(id)}, nil
}

func findPeerInfo(app *app, id peer.ID) (*codaPeerInfo, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}

	conns := app.P2p.Host.Network().ConnsToPeer(id)

	if len(conns) == 0 {
		if app.UnsafeNoTrustIP {
			app.P2p.Logger.Info("UnsafeNoTrustIP: pretending it's localhost")
			return &codaPeerInfo{Libp2pPort: 0, Host: "127.0.0.1", PeerID: peer.Encode(id)}, nil
		}
		return nil, badp2p(errors.New("tried to find peer info but no open connections to that peer ID"))
	}

	conn := conns[0]

	maybePeer, err := parseMultiaddrWithID(conn.RemoteMultiaddr(), conn.RemotePeer())
	if err != nil {
		return nil, err
	}
	return maybePeer, nil
}

type codaMetricsServer struct {
	port   string
	server *http.Server
	done   *sync.WaitGroup
}

func startMetricsServer(port string) *codaMetricsServer {
	log := logging.Logger("metrics server")
	done := &sync.WaitGroup{}
	done.Add(1)
	server := &http.Server{Addr: ":" + port}

	// does this need re-registered every time?
	// http.Handle("/metrics", promhttp.Handler())

	go func() {
		defer done.Done()

		if err := server.ListenAndServe(); err != http.ErrServerClosed {
			log.Fatalf("http server error: %v", err)
		}
	}()

	return &codaMetricsServer{
		port:   port,
		server: server,
		done:   done,
	}
}

func (ms *codaMetricsServer) Shutdown() {
	if err := ms.server.Shutdown(context.Background()); err != nil {
		panic(err)
	}

	ms.done.Wait()
}

var (
	metricsServer *codaMetricsServer
)

type configureMsg struct {
	Statedir            string             `json:"statedir"`
	Privk               string             `json:"privk"`
	NetworkID           string             `json:"network_id"`
	ListenOn            []string           `json:"ifaces"`
	MetricsPort         string             `json:"metrics_port"`
	External            string             `json:"external_maddr"`
	UnsafeNoTrustIP     bool               `json:"unsafe_no_trust_ip"`
	Flood               bool               `json:"flood"`
	PeerExchange        bool               `json:"peer_exchange"`
	DirectPeers         []string           `json:"direct_peers"`
	SeedPeers           []string           `json:"seed_peers"`
	GatingConfig        setGatingConfigMsg `json:"gating_config"`
	MaxConnections      int                `json:"max_connections"`
	ValidationQueueSize int                `json:"validation_queue_size"`
	MinaPeerExchange    bool               `json:"mina_peer_exchange"`
}

type peerConnectionUpcall struct {
	ID     string `json:"peer_id"`
	Upcall string `json:"upcall"`
}

func (m *configureMsg) run(app *app) (interface{}, error) {
	app.UnsafeNoTrustIP = m.UnsafeNoTrustIP
	privkBytes, err := codaDecode(m.Privk)
	if err != nil {
		return nil, badRPC(err)
	}
	privk, err := crypto.UnmarshalPrivateKey(privkBytes)
	if err != nil {
		return nil, badRPC(err)
	}
	maddrs := make([]multiaddr.Multiaddr, len(m.ListenOn))
	for i, v := range m.ListenOn {
		res, err := multiaddr.NewMultiaddr(v)
		if err != nil {
			return nil, badRPC(err)
		}
		maddrs[i] = res
	}

	seeds := make([]peer.AddrInfo, 0, len(m.SeedPeers))
	for _, v := range m.SeedPeers {
		addr, err := addrInfoOfString(v)
		if err != nil {
			// TODO: this isn't necessarily an RPC error. Perhaps the encoded multiaddr
			// isn't supported by this version of libp2p.
			// But more likely, it is an RPC error.
			return nil, badRPC(err)
		}
		seeds = append(seeds, *addr)
	}

	app.AddedPeers = append(app.AddedPeers, seeds...)

	directPeers := make([]peer.AddrInfo, 0, len(m.DirectPeers))
	for _, v := range m.DirectPeers {
		addr, err := addrInfoOfString(v)
		if err != nil {
			// TODO: this isn't necessarily an RPC error. Perhaps the encoded multiaddr
			// isn't supported by this version of libp2p.
			// But more likely, it is an RPC error.
			return nil, badRPC(err)
		}
		directPeers = append(directPeers, *addr)
	}

	externalMaddr, err := multiaddr.NewMultiaddr(m.External)
	if err != nil {
		return nil, badAddr(err)
	}

	gatingConfig, err := gatingConfigFromJson(&(m.GatingConfig), app.AddedPeers)
	if err != nil {
		return nil, badRPC(err)
	}

	helper, err := codanet.MakeHelper(app.Ctx, maddrs, externalMaddr, m.Statedir, privk, m.NetworkID, seeds, gatingConfig, m.MaxConnections, m.MinaPeerExchange)
	if err != nil {
		return nil, badHelper(err)
	}

	// SOMEDAY:
	// - stop putting block content on the mesh.
	// - bigger than 32MiB block size?
	opts := []pubsub.Option{pubsub.WithMaxMessageSize(1024 * 1024 * 32),
		pubsub.WithPeerExchange(m.PeerExchange),
		pubsub.WithFloodPublish(m.Flood),
		pubsub.WithDirectPeers(directPeers),
		pubsub.WithValidateQueueSize(m.ValidationQueueSize),
	}

	var ps *pubsub.PubSub
	ps, err = pubsub.NewGossipSub(app.Ctx, helper.Host, opts...)
	if err != nil {
		return nil, badHelper(err)
	}

	helper.Pubsub = ps
	app.P2p = helper

	app.P2p.Logger.Infof("here are the seeds: %v", seeds)

	if metricsServer != nil && metricsServer.port != m.MetricsPort {
		metricsServer.Shutdown()
	}
	if len(m.MetricsPort) > 0 {
		metricsServer = startMetricsServer(m.MetricsPort)
	}

	return "configure success", nil
}

type listenMsg struct {
	Iface string `json:"iface"`
}

func (m *listenMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	ma, err := multiaddr.NewMultiaddr(m.Iface)
	if err != nil {
		return nil, badp2p(err)
	}
	if err := app.P2p.Host.Network().Listen(ma); err != nil {
		return nil, badp2p(err)
	}
	return app.P2p.Host.Addrs(), nil
}

type listeningAddrsMsg struct {
}

func (m *listeningAddrsMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	return app.P2p.Host.Addrs(), nil
}

type publishMsg struct {
	Topic string `json:"topic"`
	Data  string `json:"data"`
}

func (t *publishMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	if app.P2p.Dht == nil {
		return nil, needsDHT()
	}

	data, err := codaDecode(t.Data)
	if err != nil {
		return nil, badRPC(err)
	}

	var topic *pubsub.Topic
	var has bool

	if topic, has = app.Topics[t.Topic]; !has {
		topic, err = app.P2p.Pubsub.Join(t.Topic)
		if err != nil {
			return nil, badp2p(err)
		}
		app.Topics[t.Topic] = topic
	}

	if err := topic.Publish(app.Ctx, data); err != nil {
		return nil, badp2p(err)
	}

	return "publish success", nil
}

type subscribeMsg struct {
	Topic        string `json:"topic"`
	Subscription int    `json:"subscription_idx"`
}

// we use base64 for encoding blobs in our JSON protocol. there are more
// efficient options but this one is easy to reach to.

func codaEncode(data []byte) string {
	return base64.StdEncoding.EncodeToString(data)
}

func codaDecode(data string) ([]byte, error) {
	return base64.StdEncoding.DecodeString(data)
}

var (
	acceptResult = "accept"
	rejectResult = "reject"
	ignoreResult = "ignore"
)

func (s *subscribeMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	if app.P2p.Dht == nil {
		return nil, needsDHT()
	}

	topic, err := app.P2p.Pubsub.Join(s.Topic)
	if err != nil {
		return nil, badp2p(err)
	}

	app.Topics[s.Topic] = topic

	err = app.P2p.Pubsub.RegisterTopicValidator(s.Topic, func(ctx context.Context, id peer.ID, msg *pubsub.Message) pubsub.ValidationResult {
		if id == app.P2p.Me {
			// messages from ourself are valid.
			app.P2p.Logger.Info("would have validated but it's from us!")
			return pubsub.ValidationAccept
		}

		seqno := <-seqs
		ch := make(chan string)
		app.ValidatorMutex.Lock()
		app.Validators[seqno] = new(validationStatus)
		app.Validators[seqno].Completion = ch
		app.ValidatorMutex.Unlock()

		app.P2p.Logger.Info("validating a new pubsub message ...")

		sender, err := findPeerInfo(app, id)

		if err != nil && !app.UnsafeNoTrustIP {
			app.P2p.Logger.Errorf("failed to connect to peer %s that just sent us a pubsub message, dropping it", peer.Encode(id))
			app.ValidatorMutex.Lock()
			defer app.ValidatorMutex.Unlock()
			delete(app.Validators, seqno)
			return pubsub.ValidationIgnore
		}

		deadline, ok := ctx.Deadline()
		if !ok {
			app.P2p.Logger.Errorf("no deadline set on validation context")
			defer app.ValidatorMutex.Unlock()
			delete(app.Validators, seqno)
			return pubsub.ValidationIgnore
		}

		app.writeMsg(validateUpcall{
			Sender:     sender,
			SeenAt:     time.Now().UnixNano(),
			Expiration: deadline.UnixNano(),
			Data:       codaEncode(msg.Data),
			Seqno:      seqno,
			Upcall:     "validate",
			Idx:        s.Subscription,
		})

		// Wait for the validation response, but be sure to honor any timeout/deadline in ctx
		select {
		case <-ctx.Done():
			// XXX: do 🅽🅾🆃  delete app.Validators[seqno] here! the ocaml side doesn't
			// care about the timeout and will validate it anyway.
			// validationComplete will remove app.Validators[seqno] once the
			// coda process gets around to it.
			app.P2p.Logger.Error("validation timed out :(")

			app.ValidatorMutex.Lock()

			now := time.Now()
			app.Validators[seqno].TimedOutAt = &now

			app.ValidatorMutex.Unlock()

			if app.UnsafeNoTrustIP {
				app.P2p.Logger.Info("validated anyway!")
				return pubsub.ValidationAccept
			}
			app.P2p.Logger.Info("unvalidated :(")
			return pubsub.ValidationReject
		case res := <-ch:
			switch res {
			case rejectResult:
				app.P2p.Logger.Info("why u fail to validate :(")
				return pubsub.ValidationReject
			case acceptResult:
				app.P2p.Logger.Info("validated!")
				return pubsub.ValidationAccept
			case ignoreResult:
				app.P2p.Logger.Info("ignoring valid message!")
				return pubsub.ValidationIgnore
			default:
				app.P2p.Logger.Info("ignoring message that falled off the end!")
				return pubsub.ValidationIgnore
			}
		}
	}, pubsub.WithValidatorTimeout(validationTimeout))

	if err != nil {
		return nil, badp2p(err)
	}

	sub, err := topic.Subscribe()
	if err != nil {
		return nil, badp2p(err)
	}

	ctx, cancel := context.WithCancel(app.Ctx)
	app.Subs[s.Subscription] = subscription{
		Sub:    sub,
		Idx:    s.Subscription,
		Ctx:    ctx,
		Cancel: cancel,
	}
	go func() {
		for {
			_, err = sub.Next(ctx)
			if err != nil {
				if ctx.Err() != context.Canceled {
					app.P2p.Logger.Error("sub.Next failed: ", err)
				} else {
					break
				}
			}
		}
	}()
	return "subscribe success", nil
}

type unsubscribeMsg struct {
	Subscription int `json:"subscription_idx"`
}

func (u *unsubscribeMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	if sub, ok := app.Subs[u.Subscription]; ok {
		sub.Sub.Cancel()
		sub.Cancel()
		delete(app.Subs, u.Subscription)
		return "unsubscribe success", nil
	}
	return nil, badRPC(errors.New("subscription not found"))
}

type validateUpcall struct {
	Sender     *codaPeerInfo `json:"sender"`
	SeenAt     int64         `json:"seen_at"`
	Expiration int64         `json:"expiration"`
	Data       string        `json:"data"`
	Seqno      int           `json:"seqno"`
	Upcall     string        `json:"upcall"`
	Idx        int           `json:"subscription_idx"`
}

type validationCompleteMsg struct {
	Seqno int    `json:"seqno"`
	Valid string `json:"is_valid"`
}

func (r *validationCompleteMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	app.ValidatorMutex.Lock()
	defer app.ValidatorMutex.Unlock()
	if st, ok := app.Validators[r.Seqno]; ok {
		st.Completion <- r.Valid
		if st.TimedOutAt != nil {
			app.P2p.Logger.Errorf("validation for item %d took %d seconds", r.Seqno, time.Now().Add(validationTimeout).Sub(*st.TimedOutAt))
		}
		delete(app.Validators, r.Seqno)
		return "validationComplete success", nil
	}
	return nil, badRPC(errors.New("validation seqno unknown"))
}

type generateKeypairMsg struct {
}

type generatedKeypair struct {
	Private string `json:"sk"`
	Public  string `json:"pk"`
	PeerID  string `json:"peer_id"`
}

func (*generateKeypairMsg) run(app *app) (interface{}, error) {
	privk, pubk, err := crypto.GenerateEd25519Key(cryptorand.Reader)
	if err != nil {
		return nil, badp2p(err)
	}
	privkBytes, err := crypto.MarshalPrivateKey(privk)
	if err != nil {
		return nil, badRPC(err)
	}

	pubkBytes, err := crypto.MarshalPublicKey(pubk)
	if err != nil {
		return nil, badRPC(err)
	}

	peerID, err := peer.IDFromPublicKey(pubk)
	if err != nil {
		return nil, badp2p(err)
	}

	return generatedKeypair{Private: codaEncode(privkBytes), Public: codaEncode(pubkBytes), PeerID: peer.Encode(peerID)}, nil
}

type streamLostUpcall struct {
	Upcall    string `json:"upcall"`
	StreamIdx int    `json:"stream_idx"`
	Reason    string `json:"reason"`
}

type streamReadCompleteUpcall struct {
	Upcall    string `json:"upcall"`
	StreamIdx int    `json:"stream_idx"`
}

type incomingMsgUpcall struct {
	Upcall    string `json:"upcall"`
	StreamIdx int    `json:"stream_idx"`
	Data      string `json:"data"`
}

func handleStreamReads(app *app, stream net.Stream, idx int) {
	go func() {
		defer func() {
			_ = stream.Close()
		}()

		buf := make([]byte, 4096)
		for {
			len, err := stream.Read(buf)

			if len != 0 {
				app.writeMsg(incomingMsgUpcall{
					Upcall:    "incomingStreamMsg",
					Data:      codaEncode(buf[:len]),
					StreamIdx: idx,
				})
			}

			if err != nil && err != io.EOF {
				app.writeMsg(streamLostUpcall{
					Upcall:    "streamLost",
					StreamIdx: idx,
					Reason:    fmt.Sprintf("read failure: %s", err.Error()),
				})
				break
			}

			if err == io.EOF {
				break
			}
		}
		app.writeMsg(streamReadCompleteUpcall{
			Upcall:    "streamReadComplete",
			StreamIdx: idx,
		})
	}()
}

type openStreamMsg struct {
	Peer       string `json:"peer"`
	ProtocolID string `json:"protocol"`
}

type openStreamResult struct {
	StreamIdx int          `json:"stream_idx"`
	Peer      codaPeerInfo `json:"peer"`
}

func (o *openStreamMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}

	streamIdx := <-seqs

	peer, err := peer.Decode(o.Peer)
	if err != nil {
		// TODO: this isn't necessarily an RPC error. Perhaps the encoded Peer ID
		// isn't supported by this version of libp2p.
		return nil, badRPC(err)
	}

	ctx, cancel := context.WithTimeout(app.Ctx, 30*time.Second)
	defer cancel()

	stream, err := app.P2p.Host.NewStream(ctx, peer, protocol.ID(o.ProtocolID))
	if err != nil {
		return nil, badp2p(err)
	}

	maybePeer, err := parseMultiaddrWithID(stream.Conn().RemoteMultiaddr(), stream.Conn().RemotePeer())
	if err != nil {
		_ = stream.Reset()
		return nil, badp2p(err)
	}

	app.StreamsMutex.Lock()
	defer app.StreamsMutex.Unlock()
	app.Streams[streamIdx] = stream
	go func() {
		// FIXME HACK: allow time for the openStreamResult to get printed before we start inserting stream events
		time.Sleep(250 * time.Millisecond)
		// Note: It is _very_ important that we call handleStreamReads here -- this is how the "caller" side of the stream starts listening to the responses from the RPCs. Do not remove.
		handleStreamReads(app, stream, streamIdx)
	}()
	return openStreamResult{StreamIdx: streamIdx, Peer: *maybePeer}, nil
}

type closeStreamMsg struct {
	StreamIdx int `json:"stream_idx"`
}

func (cs *closeStreamMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	app.StreamsMutex.Lock()
	defer app.StreamsMutex.Unlock()
	if stream, ok := app.Streams[cs.StreamIdx]; ok {
		delete(app.Streams, cs.StreamIdx)
		err := stream.Close()
		if err != nil {
			return nil, badp2p(err)
		}
		return "closeStream success", nil
	}
	return nil, badRPC(errors.New("unknown stream_idx"))
}

type resetStreamMsg struct {
	StreamIdx int `json:"stream_idx"`
}

func (cs *resetStreamMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	app.StreamsMutex.Lock()
	defer app.StreamsMutex.Unlock()
	if stream, ok := app.Streams[cs.StreamIdx]; ok {
		err := stream.Reset()
		delete(app.Streams, cs.StreamIdx)
		if err != nil {
			return nil, badp2p(err)
		}
		return "resetStream success", nil
	}
	return nil, badRPC(errors.New("unknown stream_idx"))
}

type sendStreamMsgMsg struct {
	StreamIdx int    `json:"stream_idx"`
	Data      string `json:"data"`
}

func (cs *sendStreamMsgMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	data, err := codaDecode(cs.Data)
	if err != nil {
		return nil, badRPC(err)
	}

	app.StreamsMutex.Lock()
	defer app.StreamsMutex.Unlock()
	if stream, ok := app.Streams[cs.StreamIdx]; ok {
		n, err := stream.Write(data)
		if err != nil {
			return nil, wrapError(badp2p(err), fmt.Sprintf("only wrote %d out of %d bytes", n, len(data)))
		}
		return "sendStreamMsg success", nil
	}
	return nil, badRPC(errors.New("unknown stream_idx"))
}

type addStreamHandlerMsg struct {
	Protocol string `json:"protocol"`
}

type incomingStreamUpcall struct {
	Upcall    string       `json:"upcall"`
	Peer      codaPeerInfo `json:"peer"`
	StreamIdx int          `json:"stream_idx"`
	Protocol  string       `json:"protocol"`
}

func (as *addStreamHandlerMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	app.P2p.Host.SetStreamHandler(protocol.ID(as.Protocol), func(stream net.Stream) {
		peerinfo, err := parseMultiaddrWithID(stream.Conn().RemoteMultiaddr(), stream.Conn().RemotePeer())
		if err != nil {
			app.P2p.Logger.Errorf("failed to parse remote connection information, silently dropping stream: %s", err.Error())
			return
		}
		streamIdx := <-seqs
		app.StreamsMutex.Lock()
		defer app.StreamsMutex.Unlock()
		app.Streams[streamIdx] = stream
		app.writeMsg(incomingStreamUpcall{
			Upcall:    "incomingStream",
			Peer:      *peerinfo,
			StreamIdx: streamIdx,
			Protocol:  as.Protocol,
		})
		handleStreamReads(app, stream, streamIdx)
	})

	return "addStreamHandler success", nil
}

type removeStreamHandlerMsg struct {
	Protocol string `json:"protocol"`
}

func (rs *removeStreamHandlerMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}
	app.P2p.Host.RemoveStreamHandler(protocol.ID(rs.Protocol))

	return "removeStreamHandler success", nil
}

type addPeerMsg struct {
	Multiaddr string `json:"multiaddr"`
	Seed      bool   `json:"seed"`
}

func addrInfoOfString(maddr string) (*peer.AddrInfo, error) {
	multiaddr, err := multiaddr.NewMultiaddr(maddr)
	if err != nil {
		return nil, err
	}
	info, err := peer.AddrInfoFromP2pAddr(multiaddr)
	if err != nil {
		return nil, err
	}

	return info, nil
}

func (ap *addPeerMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}

	info, err := addrInfoOfString(ap.Multiaddr)
	if err != nil {
		return nil, err
	}

	app.AddedPeers = append(app.AddedPeers, *info)
	app.P2p.GatingState.TrustedPeers.Add(info.ID)

	if app.Bootstrapper != nil {
		app.Bootstrapper.Close()
	}

	app.P2p.Logger.Error("addPeer Trying to connect to: ", info)

	if ap.Seed {
		app.P2p.Seeds = append(app.P2p.Seeds, *info)
	}

	err = app.P2p.Host.Connect(app.Ctx, *info)
	if err != nil {
		return nil, badp2p(err)
	}

	return "addPeer success", nil
}

type beginAdvertisingMsg struct {
}

type peerDisoverySource int

const (
	PEER_DISCOVERY_SOURCE_MDNS peerDisoverySource = iota
	PEER_DISCOVERY_SOURCE_ROUTING
)

func (source peerDisoverySource) String() string {
	switch source {
	case PEER_DISCOVERY_SOURCE_MDNS:
		return "PEER_DISCOVERY_SOURCE_MDNS"
	case PEER_DISCOVERY_SOURCE_ROUTING:
		return "PEER_DISCOVERY_SOURCE_ROUTING"
	default:
		return fmt.Sprintf("%d", int(source))
	}
}

type peerDiscovery struct {
	info   peer.AddrInfo
	source peerDisoverySource
}

type mdnsListener struct {
	FoundPeer chan peerDiscovery
	app       *app
}

func (l *mdnsListener) HandlePeerFound(info peer.AddrInfo) {
	l.FoundPeer <- peerDiscovery{
		info:   info,
		source: PEER_DISCOVERY_SOURCE_MDNS,
	}
}

func beginMDNS(app *app, foundPeerCh chan peerDiscovery) error {
	mdns, err := mdns.NewMdnsService(app.Ctx, app.P2p.Host, time.Minute, "_coda-discovery._udp.local")
	if err != nil {
		return err
	}
	app.P2p.Mdns = &mdns
	l := &mdnsListener{
		FoundPeer: foundPeerCh,
		app:       app,
	}
	mdns.RegisterNotifee(l)

	return nil
}

func (ap *beginAdvertisingMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}

	for _, info := range app.AddedPeers {
		app.P2p.Logger.Debug("Trying to connect to: ", info)
		err := app.P2p.Host.Connect(app.Ctx, info)
		if err != nil {
			app.P2p.Logger.Error("failed to connect to peer: ", info, err.Error())
			continue
		}
	}

	foundPeerCh := make(chan peerDiscovery)

	validPeer := func(who peer.ID) bool {
		return who.Validate() == nil && who != app.P2p.Me
	}

	// report discovery peers local and remote
	go func() {
		for discovery := range foundPeerCh {
			if validPeer(discovery.info.ID) {
				app.P2p.Logger.Debugf("discovered peer %v via %v; processing", discovery.info.ID, discovery.source)
				app.P2p.Host.Peerstore().AddAddrs(discovery.info.ID, discovery.info.Addrs, peerstore.ConnectedAddrTTL)

				if discovery.source == PEER_DISCOVERY_SOURCE_MDNS {
					for _, addr := range discovery.info.Addrs {
						app.P2p.GatingState.MarkPrivateAddrAsKnown(addr)
					}
				}

				// now connect to the peer we discovered
				connInfo := app.P2p.ConnectionManager.GetInfo()
				if connInfo.ConnCount < connInfo.LowWater {
					err := app.P2p.Host.Connect(app.Ctx, discovery.info)
					if err != nil {
						app.P2p.Logger.Error("failed to connect to peer after discovering it: ", discovery.info, err.Error())
						continue
					}
				}
			} else {
				app.P2p.Logger.Debugf("discovered peer %v via %v; not processing as it is not a valid peer", discovery.info.ID, discovery.source)
			}
		}
	}()

	if !app.NoMDNS {
		app.P2p.Logger.Infof("beginning mDNS discovery")
		err := beginMDNS(app, foundPeerCh)
		if err != nil {
			app.P2p.Logger.Error("failed to connect to begin mdns: ", err.Error())
			return nil, badp2p(err)
		}
	}

	if !app.NoDHT {
		app.P2p.Logger.Infof("beginning DHT discovery")
		routingDiscovery := discovery.NewRoutingDiscovery(app.P2p.Dht)
		if routingDiscovery == nil {
			return nil, errors.New("failed to create routing discovery")
		}

		app.P2p.Discovery = routingDiscovery

		err := app.P2p.Dht.Bootstrap(app.Ctx)
		if err != nil {
			app.P2p.Logger.Error("failed to dht bootstrap: ", err.Error())
			return nil, badp2p(err)
		}

		time.Sleep(time.Millisecond * 100)
		app.P2p.Logger.Debugf("beginning DHT advertising")

		_, err = routingDiscovery.Advertise(app.Ctx, app.P2p.Rendezvous)
		if err != nil {
			app.P2p.Logger.Error("failed to routing advertise: ", err.Error())
			return nil, badp2p(err)
		}

		go func() {
			peerCh, err := routingDiscovery.FindPeers(app.Ctx, app.P2p.Rendezvous)
			if err != nil {
				app.P2p.Logger.Error("error while trying to find some peers: ", err.Error())
			}

			for peer := range peerCh {
				foundPeerCh <- peerDiscovery{
					info:   peer,
					source: PEER_DISCOVERY_SOURCE_ROUTING,
				}
			}
		}()
	}

	app.P2p.ConnectionManager.OnConnect = func(net net.Network, c net.Conn) {
		app.updateConnectionMetrics()

		id := c.RemotePeer()

		app.writeMsg(peerConnectionUpcall{
			ID:     peer.Encode(id),
			Upcall: "peerConnected",
		})

		// Note: These are disabled because we see weirdness on our networks
		//       caused by this prometheus issues.
		// go app.checkBandwidth(id)
		// go app.checkLatency(id)
	}

	app.P2p.ConnectionManager.OnDisconnect = func(net net.Network, c net.Conn) {
		app.updateConnectionMetrics()

		id := c.RemotePeer()

		app.writeMsg(peerConnectionUpcall{
			ID:     peer.Encode(id),
			Upcall: "peerDisconnected",
		})
	}

	return "beginAdvertising success", nil
}

const (
	latencyMeasurementTime = time.Second * 5
	metricsRefreshTime     = time.Minute
)

func (app *app) updateConnectionMetrics() {
	info := app.P2p.ConnectionManager.GetInfo()
	connectionCountMetric.Set(float64(info.ConnCount))
}

func (a *app) checkBandwidth(id peer.ID) {
	totalIn := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: fmt.Sprintf("total_bandwidth_in_%s", id),
		Help: "The bandwidth used by the given peer.",
	})
	totalOut := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: fmt.Sprintf("total_bandwidth_out_%s", id),
		Help: "The bandwidth used by the given peer.",
	})
	rateIn := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: fmt.Sprintf("bandwidth_rate_in_%s", id),
		Help: "The bandwidth used by the given peer.",
	})
	rateOut := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: fmt.Sprintf("bandwidth_rate_out_%s", id),
		Help: "The bandwidth used by the given peer.",
	})

	err := prometheus.Register(totalIn)
	if err != nil {
		a.P2p.Logger.Debugf("couldn't register total-in bandwidth gauge for id", id, "perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(totalOut)
	if err != nil {
		a.P2p.Logger.Debugf("couldn't register total-out bandwidth gauge for id", id, "perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(rateIn)
	if err != nil {
		a.P2p.Logger.Debugf("couldn't register rate-in bandwidth gauge for id", id, "perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(rateOut)
	if err != nil {
		a.P2p.Logger.Debugf("couldn't register rate-out bandwidth gauge for id", id, "perhaps we've already done so", err.Error())
		return
	}

	for {
		stats := a.P2p.BandwidthCounter.GetBandwidthForPeer(id)
		totalIn.Set(float64(stats.TotalIn))
		totalOut.Set(float64(stats.TotalOut))
		rateIn.Set(stats.RateIn)
		rateOut.Set(stats.RateOut)

		time.Sleep(metricsRefreshTime)
	}
}

func (a *app) checkLatency(id peer.ID) {
	latencyGauge := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: fmt.Sprintf("latency_%s", id),
		Help: "The latency for the given peer.",
	})

	err := prometheus.Register(latencyGauge)
	if err != nil {
		a.P2p.Logger.Debugf("couldn't register latency gauge for id", id, "perhaps we've already done so", err.Error())
		return
	}

	for {
		a.P2p.Host.Peerstore().RecordLatency(id, latencyMeasurementTime)
		latency := a.P2p.Host.Peerstore().LatencyEWMA(id)
		latencyGauge.Set(float64(latency))

		time.Sleep(metricsRefreshTime)
	}
}

type findPeerMsg struct {
	PeerID string `json:"peer_id"`
}

func (ap *findPeerMsg) run(app *app) (interface{}, error) {
	id, err := peer.Decode(ap.PeerID)
	if err != nil {
		return nil, err
	}

	maybePeer, err := findPeerInfo(app, id)

	if err != nil {
		return nil, err
	}

	return *maybePeer, nil
}

type setNodeStatusMsg struct {
	Data string `json:"data"`
}

func (m *setNodeStatusMsg) run(app *app) (interface{}, error) {
	app.P2p.NodeStatus = m.Data
	return "setNodeStatus success", nil
}

type getPeerNodeStatusMsg struct {
	PeerMultiaddr string `json:"peer_multiaddr"`
}

func (m *getPeerNodeStatusMsg) run(app *app) (interface{}, error) {
	ctx, _ := context.WithTimeout(app.Ctx, codanet.NodeStatusTimeout)

	addrInfo, err := addrInfoOfString(m.PeerMultiaddr)
	if err != nil {
		return nil, err
	}

	app.P2p.Host.Peerstore().AddAddrs(addrInfo.ID, addrInfo.Addrs, peerstore.ConnectedAddrTTL)

	// Open a "get node status" stream on m.PeerID,
	// block until you can read the response, return that.
	s, err := app.P2p.Host.NewStream(ctx, addrInfo.ID, codanet.NodeStatusProtocolID)
	if err != nil {
		app.P2p.Logger.Error("failed to open stream: ", err)
		return nil, err
	}

	defer func() {
		_ = s.Close()
	}()

	errCh := make(chan error)
	responseCh := make(chan string)

	go func() {
		// 1 megabyte
		size := 1048576

		data := make([]byte, size)
		n, err := s.Read(data)
		if err != nil && err != io.EOF {
			app.P2p.Logger.Errorf("failed to decode node status data: err=%s", err)
			errCh <- err
			return
		}

		if n == size && err == nil {
			errCh <- fmt.Errorf("node status data was greater than %d bytes", size)
			return
		}

		responseCh <- string(data[:n])
	}()

	select {
	case <-ctx.Done():
		s.Reset()
		return nil, errors.New("timed out requesting node status data from peer")
	case err := <-errCh:
		return nil, err
	case response := <-responseCh:
		return response, nil
	}
}

type listPeersMsg struct {
}

func (lp *listPeersMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}

	connsHere := app.P2p.Host.Network().Conns()

	peerInfos := make([]codaPeerInfo, 0, len(connsHere))

	for _, conn := range connsHere {
		maybePeer, err := parseMultiaddrWithID(conn.RemoteMultiaddr(), conn.RemotePeer())
		if err != nil {
			app.P2p.Logger.Warning("skipping maddr ", conn.RemoteMultiaddr().String(), " because it failed to parse: ", err.Error())
			continue
		}
		peerInfos = append(peerInfos, *maybePeer)
	}

	return peerInfos, nil
}

func filterIPString(filters *ma.Filters, ip string, action ma.Action) error {
	realIP := gonet.ParseIP(ip).To4()

	if realIP == nil {
		// TODO: how to compute mask for IPv6?
		return badRPC(errors.New("unparsable IP or IPv6"))
	}

	ipnet := gonet.IPNet{Mask: gonet.IPv4Mask(255, 255, 255, 255), IP: realIP}

	filters.AddFilter(ipnet, action)

	return nil
}

type setGatingConfigMsg struct {
	BannedIPs      []string `json:"banned_ips"`
	BannedPeerIDs  []string `json:"banned_peers"`
	TrustedPeerIDs []string `json:"trusted_peers"`
	TrustedIPs     []string `json:"trusted_ips"`
	Isolate        bool     `json:"isolate"`
}

func gatingConfigFromJson(gc *setGatingConfigMsg, addedPeers []peer.AddrInfo) (*codanet.CodaGatingState, error) {
	_, totalIpNet, err := gonet.ParseCIDR("0.0.0.0/0")
	if err != nil {
		return nil, err
	}

	// TODO: perhaps the isolate option should just be passed down to the gating state instead
	bannedAddrFilters := ma.NewFilters()
	if gc.Isolate {
		bannedAddrFilters.AddFilter(*totalIpNet, ma.ActionDeny)
	}
	for _, ip := range gc.BannedIPs {
		err := filterIPString(bannedAddrFilters, ip, ma.ActionDeny)
		if err != nil {
			return nil, err
		}
	}

	trustedAddrFilters := ma.NewFilters()
	trustedAddrFilters.AddFilter(*totalIpNet, ma.ActionDeny)
	for _, ip := range gc.TrustedIPs {
		err := filterIPString(trustedAddrFilters, ip, ma.ActionAccept)
		if err != nil {
			return nil, err
		}
	}

	bannedPeers := peer.NewSet()
	for _, peerID := range gc.BannedPeerIDs {
		id := peer.ID(peerID)
		bannedPeers.Add(id)
	}

	trustedPeers := peer.NewSet()
	for _, peerID := range gc.TrustedPeerIDs {
		id := peer.ID(peerID)
		trustedPeers.Add(id)
	}
	for _, peer := range addedPeers {
		trustedPeers.Add(peer.ID)
	}

	return codanet.NewCodaGatingState(bannedAddrFilters, trustedAddrFilters, bannedPeers, trustedPeers), nil
}

func (gc *setGatingConfigMsg) run(app *app) (interface{}, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}

	newState, err := gatingConfigFromJson(gc, app.AddedPeers)
	if err != nil {
		return nil, badRPC(err)
	}

	app.P2p.GatingState = newState

	return "ok", nil
}

var msgHandlers = map[methodIdx]func() action{
	configure:            func() action { return &configureMsg{} },
	listen:               func() action { return &listenMsg{} },
	publish:              func() action { return &publishMsg{} },
	subscribe:            func() action { return &subscribeMsg{} },
	unsubscribe:          func() action { return &unsubscribeMsg{} },
	validationComplete:   func() action { return &validationCompleteMsg{} },
	generateKeypair:      func() action { return &generateKeypairMsg{} },
	openStream:           func() action { return &openStreamMsg{} },
	closeStream:          func() action { return &closeStreamMsg{} },
	resetStream:          func() action { return &resetStreamMsg{} },
	sendStreamMsg:        func() action { return &sendStreamMsgMsg{} },
	removeStreamHandler:  func() action { return &removeStreamHandlerMsg{} },
	addStreamHandler:     func() action { return &addStreamHandlerMsg{} },
	listeningAddrs:       func() action { return &listeningAddrsMsg{} },
	addPeer:              func() action { return &addPeerMsg{} },
	beginAdvertising:     func() action { return &beginAdvertisingMsg{} },
	findPeer:             func() action { return &findPeerMsg{} },
	listPeers:            func() action { return &listPeersMsg{} },
	setGatingConfig:      func() action { return &setGatingConfigMsg{} },
	setNodeStatus:        func() action { return &setNodeStatusMsg{} },
	getPeerNodeStatus:    func() action { return &getPeerNodeStatusMsg{} },
}

type errorResult struct {
	Seqno  int    `json:"seqno"`
	Errorr string `json:"error"`
}

type successResult struct {
	Seqno    int             `json:"seqno"`
	Success  json.RawMessage `json:"success"`
	Duration string          `json:"duration"`
}

var connectionCountMetric = prometheus.NewGauge(prometheus.GaugeOpts{
	Name: "Coda_active_connections_total",
	Help: "Number of active connections, according to the CodaConnectionManager.",
})

func init() {
	// === Register metrics collectors here ===
	prometheus.MustRegister(connectionCountMetric)
	http.Handle("/metrics", promhttp.Handler())
}

func newApp() *app {
	return &app{
		P2p:            nil,
		Ctx:            context.Background(),
		Subs:           make(map[int]subscription),
		Topics:         make(map[string]*pubsub.Topic),
		ValidatorMutex: &sync.Mutex{},
		Validators:     make(map[int]*validationStatus),
		Streams:        make(map[int]net.Stream),
		OutChan:        make(chan interface{}, 4096),
		Out:            bufio.NewWriter(os.Stdout),
		AddedPeers:     []peer.AddrInfo{},
	}
}

func main() {
	logging.SetupLogging(logging.Config{
		Format: logging.JSONOutput,
		Stderr: true,
		Stdout: false,
		Level:  logging.LevelDebug,
		File:   "",
	})
	helperLog := logging.Logger("helper top-level JSON handling")

	helperLog.Infof("libp2p_helper has the following logging subsystems active: %v", logging.GetSubsystems())

	// === Set subsystem log levels ===
	// All subsystems that have been considered are explicitly listed. Any that
	// are added when modifying this code should be considered and added to
	// this list.
	// The levels below set the **minimum** log level for each subsystem.
	// Messages emitted at lower levels than the given level will not be
	// emitted.
	_ = logging.SetLogLevel("mplex", "debug")
	_ = logging.SetLogLevel("addrutil", "info")     // Logs every resolve call at debug
	_ = logging.SetLogLevel("net/identify", "info") // Logs every message sent/received at debug
	_ = logging.SetLogLevel("ping", "info")         // Logs every ping timeout at debug
	_ = logging.SetLogLevel("basichost", "info")    // Spammy at debug
	_ = logging.SetLogLevel("test-logger", "debug")
	_ = logging.SetLogLevel("blankhost", "debug")
	_ = logging.SetLogLevel("connmgr", "debug")
	_ = logging.SetLogLevel("eventlog", "debug")
	_ = logging.SetLogLevel("p2p-config", "debug")
	_ = logging.SetLogLevel("ipns", "debug")
	_ = logging.SetLogLevel("nat", "debug")
	_ = logging.SetLogLevel("autorelay", "info") // Logs relayed byte counts spammily
	_ = logging.SetLogLevel("providers", "debug")
	_ = logging.SetLogLevel("dht/RtRefreshManager", "warn") // Ping logs are spammy at debug, cpl logs are spammy at info
	_ = logging.SetLogLevel("dht", "info")                  // Logs every operation to debug
	_ = logging.SetLogLevel("peerstore", "debug")
	_ = logging.SetLogLevel("diversityFilter", "debug")
	_ = logging.SetLogLevel("table", "debug")
	_ = logging.SetLogLevel("stream-upgrader", "debug")
	_ = logging.SetLogLevel("helper top-level JSON handling", "debug")
	_ = logging.SetLogLevel("dht.pb", "debug")
	_ = logging.SetLogLevel("tcp-tpt", "debug")
	_ = logging.SetLogLevel("autonat", "debug")
	_ = logging.SetLogLevel("discovery", "debug")
	_ = logging.SetLogLevel("routing/record", "debug")
	_ = logging.SetLogLevel("pubsub", "debug") // Spammy about blacklisted peers, maybe should be info?
	_ = logging.SetLogLevel("badger", "debug")
	_ = logging.SetLogLevel("relay", "info") // Log relayed byte counts spammily
	_ = logging.SetLogLevel("routedhost", "debug")
	_ = logging.SetLogLevel("swarm2", "info") // Logs a new stream to each peer when opended at debug
	_ = logging.SetLogLevel("peerstore/ds", "debug")
	_ = logging.SetLogLevel("mdns", "info") // Logs each mdns call
	_ = logging.SetLogLevel("bootstrap", "debug")
	_ = logging.SetLogLevel("reuseport-transport", "debug")

	go func() {
		i := 0
		for {
			seqs <- i
			i++
		}
	}()

	lines := bufio.NewScanner(os.Stdin)
	// 22MiB buffer size, larger than the 21.33MB minimum for 16MiB to be b64'd
	// 4 * (2^24/3) / 2^20 = 21.33
	bufsize := (1024 * 1024) * 1024
	lines.Buffer(make([]byte, bufsize), bufsize)

	app := newApp()

	go func() {
		for {
			msg := <-app.OutChan
			bytes, err := json.Marshal(msg)
			if err != nil {
				panic(err)
			}

			n, err := app.Out.Write(bytes)
			if err != nil {
				panic(err)
			}

			if n != len(bytes) {
				// TODO: handle this correctly.
				panic("short write :(")
			}

			err = app.Out.WriteByte(0x0a)
			if err != nil {
				panic(err)
			}

			if err := app.Out.Flush(); err != nil {
				panic(err)
			}
		}
	}()

	var line string

	defer func() {
		if r := recover(); r != nil {
			helperLog.Error("While handling RPC:", line, "\nThe following panic occurred: ", r, "\nstack:\n", string(debug.Stack()))
		}
	}()

	for lines.Scan() {
		line = lines.Text()
		helperLog.Debugf("message size is %d", len(line))
		var raw json.RawMessage
		env := envelope{
			Body: &raw,
		}
		if err := json.Unmarshal([]byte(line), &env); err != nil {
			log.Print("when unmarshaling the envelope...")
			log.Panic(err)
		}
		msg := msgHandlers[env.Method]()
		if err := json.Unmarshal(raw, msg); err != nil {
			log.Print("when unmarshaling the method invocation...")
			log.Panic(err)
		}

		go func() {
			start := time.Now()
			ret, err := msg.run(app)
			if err != nil {
				app.writeMsg(errorResult{Seqno: env.Seqno, Errorr: err.Error()})
				return
			}

			res, err := json.Marshal(ret)
			if err != nil {
				app.writeMsg(errorResult{Seqno: env.Seqno, Errorr: err.Error()})
				return
			}

			app.writeMsg(successResult{Seqno: env.Seqno, Success: res, Duration: time.Since(start).String()})
		}()
	}
	app.writeMsg(errorResult{Seqno: 0, Errorr: fmt.Sprintf("helper stdin scanning stopped because %v", lines.Err())})
	// we never want the helper to get here, it should be killed or gracefully
	// shut down instead of stdin closed.
	os.Exit(1)
}

var _ json.Marshaler = (*methodIdx)(nil)
