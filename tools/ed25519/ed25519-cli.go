package main

import (
    "bytes"
    "crypto/ed25519"
    "crypto/rand"
    "encoding/hex"
    "errors"
    "flag"
    "fmt"
    "io"
    "os"
    "strings"
)

func writeFile(path string, data []byte, perm os.FileMode) error {
    if err := os.MkdirAll(dir(path), 0o755); err != nil {
        return err
    }
    return os.WriteFile(path, data, perm)
}

func dir(p string) string {
    if i := strings.LastIndexAny(p, "/\\"); i >= 0 {
        return p[:i]
    }
    return "."
}

func cmdGen(privPath, pubPath string) error {
    pub, priv, err := ed25519.GenerateKey(rand.Reader)
    if err != nil {
        return err
    }
    if err := writeFile(privPath, []byte(hex.EncodeToString(priv)), 0o600); err != nil {
        return err
    }
    if err := writeFile(pubPath, []byte(hex.EncodeToString(pub)), 0o644); err != nil {
        return err
    }
    fmt.Println("ok: generated")
    return nil
}

func loadHexFile(path string, expectedLen int) ([]byte, error) {
    b, err := os.ReadFile(path)
    if err != nil {
        return nil, err
    }
    b = bytes.TrimSpace(b)
    raw, err := hex.DecodeString(string(b))
    if err != nil {
        return nil, err
    }
    if expectedLen > 0 && len(raw) != expectedLen {
        return nil, fmt.Errorf("length mismatch: have %d want %d", len(raw), expectedLen)
    }
    return raw, nil
}

func cmdSign(privPath, msgHex string) error {
    priv, err := loadHexFile(privPath, ed25519.PrivateKeySize)
    if err != nil {
        return err
    }
    msg, err := hex.DecodeString(strings.TrimSpace(msgHex))
    if err != nil {
        return err
    }
    sig := ed25519.Sign(ed25519.PrivateKey(priv), msg)
    fmt.Print(hex.EncodeToString(sig))
    return nil
}

func cmdVerify(pubPath, msgHex, sigHex string) error {
    pub, err := loadHexFile(pubPath, ed25519.PublicKeySize)
    if err != nil {
        return err
    }
    msg, err := hex.DecodeString(strings.TrimSpace(msgHex))
    if err != nil {
        return err
    }
    sig, err := hex.DecodeString(strings.TrimSpace(sigHex))
    if err != nil {
        return err
    }
    ok := ed25519.Verify(ed25519.PublicKey(pub), msg, sig)
    if !ok {
        return errors.New("verify: false")
    }
    fmt.Print("true")
    return nil
}

func main() {
    genCmd := flag.NewFlagSet("gen", flag.ExitOnError)
    genPriv := genCmd.String("priv", ".keys/ed25519.private", "private key path")
    genPub := genCmd.String("pub", ".keys/ed25519.public", "public key path")

    signCmd := flag.NewFlagSet("sign", flag.ExitOnError)
    signPriv := signCmd.String("priv", ".keys/ed25519.private", "private key path")
    signMsg := signCmd.String("msg", "", "message hex (required)")

    verifyCmd := flag.NewFlagSet("verify", flag.ExitOnError)
    verPub := verifyCmd.String("pub", ".keys/ed25519.public", "public key path")
    verMsg := verifyCmd.String("msg", "", "message hex (required)")
    verSig := verifyCmd.String("sig", "", "signature hex (required)")

    if len(os.Args) < 2 {
        fmt.Fprintf(os.Stderr, "usage: %s <gen|sign|verify> [flags]\n", os.Args[0])
        os.Exit(2)
    }
    switch os.Args[1] {
    case "gen":
        genCmd.Parse(os.Args[2:])
        if err := cmdGen(*genPriv, *genPub); err != nil {
            fmt.Fprintln(os.Stderr, "error:", err)
            os.Exit(1)
        }
    case "sign":
        signCmd.Parse(os.Args[2:])
        if *signMsg == "" {
            fmt.Fprintln(os.Stderr, "error: -msg is required")
            os.Exit(1)
        }
        if err := cmdSign(*signPriv, *signMsg); err != nil {
            fmt.Fprintln(os.Stderr, "error:", err)
            os.Exit(1)
        }
    case "verify":
        verifyCmd.Parse(os.Args[2:])
        if *verMsg == "" || *verSig == "" {
            fmt.Fprintln(os.Stderr, "error: -msg and -sig are required")
            os.Exit(1)
        }
        if err := cmdVerify(*verPub, *verMsg, *verSig); err != nil {
            fmt.Fprintln(os.Stderr, "false")
            os.Exit(1)
        }
    default:
        fmt.Fprintln(os.Stderr, "unknown subcommand")
        os.Exit(2)
    }
    _ = io.Discard
}
