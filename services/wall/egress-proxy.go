package main

import (
    "context"
    "crypto/tls"
    "encoding/hex"
    "fmt"
    "io"
    "log"
    "net"
    "net/http"
    "net/url"
    "os"
    "path/filepath"
    "strings"
    "time"
)

func splitList(env string) []string {
    v := strings.TrimSpace(os.Getenv(env))
    if v == "" { return nil }
    parts := strings.Split(v, ",")
    out := make([]string, 0, len(parts))
    for _, p := range parts {
        p = strings.TrimSpace(p)
        if p != "" { out = append(out, p) }
    }
    return out
}

func matchHost(host string, patterns []string) bool {
    for _, p := range patterns {
        // support wildcard like *.example.com
        if strings.Contains(p, "*") {
            pat := strings.ReplaceAll(p, "*", "*")
            ok, _ := filepath.Match(pat, host)
            if ok { return true }
        } else if strings.EqualFold(p, host) {
            return true
        }
    }
    return false
}

func allowed(u *url.URL, allow, deny []string) error {
    if u == nil { return fmt.Errorf("nil url") }
    if u.Scheme != "https" {
        return fmt.Errorf("only https allowed")
    }
    host := u.Hostname()
    if matchHost(host, deny) {
        return fmt.Errorf("host denied by policy: %s", host)
    }
    if len(allow) > 0 && !matchHost(host, allow) {
        return fmt.Errorf("host not on allowlist: %s", host)
    }
    return nil
}

func redactHeaders(h http.Header, redact []string) http.Header {
    if len(redact) == 0 { return h }
    out := h.Clone()
    for _, k := range redact {
        out.Del(k)
    }
    return out
}

func main() {
    addr := os.Getenv("WALL_LISTEN")
    if addr == "" { addr = ":8088" }

    allow := splitList("ALLOW_DOMAINS")
    deny := splitList("DENY_DOMAINS")
    redact := splitList("REDACT_HEADERS")

    dialer := &net.Dialer{Timeout: 10 * time.Second}
    client := &http.Client{
        Timeout: 60 * time.Second,
        Transport: &http.Transport{
            Proxy:               nil,
            DialContext:         dialer.DialContext,
            TLSHandshakeTimeout: 10 * time.Second,
            ForceAttemptHTTP2:   true,
            TLSClientConfig:     &tls.Config{MinVersion: tls.VersionTLS12},
            DisableCompression:  false,
            MaxIdleConns:        100,
            IdleConnTimeout:     30 * time.Second,
        },
    }

    mux := http.NewServeMux()

    // Health
    mux.HandleFunc("/healthz", func(w http.ResponseWriter, r *http.Request) {
        w.WriteHeader(200)
        _, _ = w.Write([]byte("ok"))
    })

    // Simple fetch endpoint: GET /fetch?url=<https-url>
    mux.HandleFunc("/fetch", func(w http.ResponseWriter, r *http.Request) {
        if r.Method != http.MethodGet && r.Method != http.MethodHead {
            http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
            return
        }
        raw := r.URL.Query().Get("url")
        u, err := url.Parse(raw)
        if err != nil {
            http.Error(w, "bad url", http.StatusBadRequest)
            return
        }
        if err := allowed(u, allow, deny); err != nil {
            http.Error(w, err.Error(), http.StatusForbidden)
            return
        }

        // Build upstream request
        ctx, cancel := context.WithTimeout(r.Context(), 60*time.Second)
        defer cancel()
        req, err := http.NewRequestWithContext(ctx, http.MethodGet, u.String(), nil)
        if err != nil {
            http.Error(w, "bad upstream request", http.StatusInternalServerError)
            return
        }
        // Minimal headers; no auth forwarded
        req.Header = redactHeaders(http.Header{
            "User-Agent": []string{"BitHub-Wall/1.0"},
            "Accept":     []string{"*/*"},
        }, redact)

        resp, err := client.Do(req)
        if err != nil {
            http.Error(w, "upstream error: "+err.Error(), http.StatusBadGateway)
            return
        }
        defer resp.Body.Close()

        // Mirror content type and status
        for k, vals := range resp.Header {
            // Avoid hop-by-hop headers
            if strings.EqualFold(k, "Set-Cookie") { continue }
            for _, v := range vals {
                w.Header().Add(k, v)
            }
        }
        w.WriteHeader(resp.StatusCode)
        _, _ = io.Copy(w, resp.Body)
    })

    // Echo endpoint for quick tests
    mux.HandleFunc("/echo", func(w http.ResponseWriter, r *http.Request) {
        msg := r.URL.Query().Get("m")
        if msg == "" { msg = "hello" }
        sum := hex.EncodeToString([]byte(msg))
        w.WriteHeader(200)
        _, _ = w.Write([]byte(sum))
    })

    srv := &http.Server{
        Addr:              addr,
        Handler:           mux,
        ReadTimeout:       10 * time.Second,
        ReadHeaderTimeout: 10 * time.Second,
        WriteTimeout:      90 * time.Second,
        IdleTimeout:       60 * time.Second,
    }

    log.Printf("egress-proxy listening on %s", addr)
    if err := srv.ListenAndServe(); err != nil {
        log.Fatal(err)
    }
}
