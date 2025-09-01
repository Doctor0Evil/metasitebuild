package main

import (
    "crypto/ed25519"
    "encoding/hex"
    "encoding/json"
    "flag"
    "fmt"
    "log"
    "net"
    "strings"
    "sync"
    "time"
)

type Topic string
const (
    TopicLedger     Topic = "ledger"
    TopicAssetIndex Topic = "asset-index"
)

type Message struct {
    Topic   Topic   `json:"topic"`
    Payload []byte  `json:"payload"` // raw json line or index blob
    PubKey  string  `json:"pubkey"`  // hex
    Sign    string  `json:"sign"`    // hex
}

var (
    peers    = flag.String("peers", "", "peer list host:port,host:port")
    listen   = flag.String("listen", ":7777", "listen addr")
    topics   = flag.String("topics", "ledger,asset-index", "topics to accept")
    pubCache sync.Map
)

func verify(pubhex, msgh, sigh string) bool {
    pub, err := hex.DecodeString(strings.TrimSpace(pubhex))
    if err != nil { return false }
    msg, err := hex.DecodeString(strings.TrimSpace(msgh))
    if err != nil { return false }
    sig, err := hex.DecodeString(strings.TrimSpace(sigh))
    if err != nil { return false }
    return ed25519.Verify(ed25519.PublicKey(pub), msg, sig)
}

func handle(conn net.Conn, allowed map[Topic]bool) {
    defer conn.Close()
    dec := json.NewDecoder(conn)
    for {
        var m Message
        if err := dec.Decode(&m); err != nil { return }
        if !allowed[m.Topic] { continue }
        // Payload should contain its own hash; require valid signature
        type Wire struct {
            PayloadHash string `json:"payload_hash"`
        }
        var w Wire
        json.Unmarshal(m.Payload, &w)
        if w.PayloadHash == "" || !verify(m.PubKey, w.PayloadHash, m.Sign) {
            continue
        }
        // Persist by topic (append or replace) — stubbed to stdout
        fmt.Printf("RECV %s %s\n", m.Topic, w.PayloadHash)
    }
}

func main() {
    flag.Parse()
    allowed := map[Topic]bool{}
    for _, t := range strings.Split(*topics, ",") {
        allowed[Topic(strings.TrimSpace(t))] = true
    }
    ln, err := net.Listen("tcp", *listen)
    if err != nil { log.Fatal(err) }
    go func() {
        for {
          // naive gossip tick — in production, add peer dialing and fanout
            time.Sleep(10 * time.Second)
        }
    }()
    for {
        c, err := ln.Accept()
        if err != nil { continue }
        go handle(c, allowed)
    }
}
