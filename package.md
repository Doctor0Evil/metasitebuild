pkg bytes, func ToValidUTF8([]uint8, []uint8) []uint8

pkg crypto/ed25519, const PrivateKeySize = 64

pkg crypto/ed25519, const PrivateKeySize ideal-int

pkg crypto/ed25519, const PublicKeySize = 32

pkg crypto/ed25519, const PublicKeySize ideal-int

pkg crypto/ed25519, const SeedSize = 32

pkg crypto/ed25519, const SeedSize ideal-int

pkg crypto/ed25519, const SignatureSize = 64

pkg crypto/ed25519, const SignatureSize ideal-int

pkg crypto/ed25519, func GenerateKey(io.Reader) (PublicKey, PrivateKey, error)

pkg crypto/ed25519, func NewKeyFromSeed([]uint8) PrivateKey

pkg crypto/ed25519, func Sign(PrivateKey, []uint8) []uint8

pkg crypto/ed25519, func Verify(PublicKey, []uint8, []uint8) bool

pkg crypto/ed25519, method (PrivateKey) Public() crypto.PublicKey

pkg crypto/ed25519, method (PrivateKey) Seed() []uint8

pkg crypto/ed25519, method (PrivateKey) Sign(io.Reader, []uint8, crypto.SignerOpts) ([]uint8, error)

pkg crypto/ed25519, type PrivateKey []uint8

pkg crypto/ed25519, type PublicKey []uint8

pkg crypto/tls, const Ed25519 = 2055

pkg crypto/tls, const Ed25519 SignatureScheme

pkg crypto/x509, const Ed25519 = 4

pkg crypto/x509, const Ed25519 PublicKeyAlgorithm

pkg crypto/x509, const PureEd25519 = 16

pkg crypto/x509, const PureEd25519 SignatureAlgorithm

pkg database/sql, method (*Conn) Raw(func(interface{}) error) error

pkg database/sql, method (*NullInt32) Scan(interface{}) error

pkg database/sql, method (NullInt32) Value() (driver.Value, error)

pkg database/sql, method (*NullTime) Scan(interface{}) error

pkg database/sql, method (NullTime) Value() (driver.Value, error)

pkg database/sql, type NullInt32 struct

pkg database/sql, type NullInt32 struct, Int32 int32

pkg database/sql, type NullInt32 struct, Valid bool

pkg database/sql, type NullTime struct

pkg database/sql, type NullTime struct, Time time.Time

pkg database/sql, type NullTime struct, Valid bool

pkg debug/dwarf, method (*UnsupportedType) Common() *CommonType

pkg debug/dwarf, method (*UnsupportedType) Size() int64

pkg debug/dwarf, method (*UnsupportedType) String() string

pkg debug/dwarf, type UnsupportedType struct

pkg debug/dwarf, type UnsupportedType struct, embedded CommonType

pkg debug/dwarf, type UnsupportedType struct, Tag Tag

pkg debug/elf, type Symbol struct, Library string

pkg debug/elf, type Symbol struct, Version string

pkg encoding/csv, method (*ParseError) Unwrap() error

pkg encoding/json, method (*MarshalerError) Unwrap() error

pkg errors, func As(error, interface{}) bool

pkg errors, func Is(error, error) bool

pkg errors, func Unwrap(error) error

pkg go/constant, func Make(interface{}) Value

pkg go/constant, func Val(Value) interface{}

pkg go/token, func IsExported(string) bool

pkg go/token, func IsIdentifier(string) bool

pkg go/token, func IsKeyword(string) bool

pkg go/types, func CheckExpr(*token.FileSet, *Package, token.Pos, ast.Expr, *Info) error

pkg log, func Writer() io.Writer

pkg log/syslog (netbsd-arm64-cgo), const LOG_ALERT = 1

pkg log/syslog (netbsd-arm64-cgo), const LOG_ALERT Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_AUTH = 32

pkg log/syslog (netbsd-arm64-cgo), const LOG_AUTH Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_AUTHPRIV = 80

pkg log/syslog (netbsd-arm64-cgo), const LOG_AUTHPRIV Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_CRIT = 2

pkg log/syslog (netbsd-arm64-cgo), const LOG_CRIT Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_CRON = 72

pkg log/syslog (netbsd-arm64-cgo), const LOG_CRON Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_DAEMON = 24

pkg log/syslog (netbsd-arm64-cgo), const LOG_DAEMON Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_DEBUG = 7

pkg log/syslog (netbsd-arm64-cgo), const LOG_DEBUG Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_EMERG = 0

pkg log/syslog (netbsd-arm64-cgo), const LOG_EMERG Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_ERR = 3

pkg log/syslog (netbsd-arm64-cgo), const LOG_ERR Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_FTP = 88

pkg log/syslog (netbsd-arm64-cgo), const LOG_FTP Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_INFO = 6

pkg log/syslog (netbsd-arm64-cgo), const LOG_INFO Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_KERN = 0

pkg log/syslog (netbsd-arm64-cgo), const LOG_KERN Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL0 = 128

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL0 Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL1 = 136

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL1 Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL2 = 144

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL2 Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL3 = 152

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL3 Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL4 = 160

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL4 Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL5 = 168

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL5 Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL6 = 176

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL6 Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL7 = 184

pkg log/syslog (netbsd-arm64-cgo), const LOG_LOCAL7 Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_LPR = 48

pkg log/syslog (netbsd-arm64-cgo), const LOG_LPR Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_MAIL = 16

pkg log/syslog (netbsd-arm64-cgo), const LOG_MAIL Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_NEWS = 56

pkg log/syslog (netbsd-arm64-cgo), const LOG_NEWS Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_NOTICE = 5

pkg log/syslog (netbsd-arm64-cgo), const LOG_NOTICE Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_SYSLOG = 40

pkg log/syslog (netbsd-arm64-cgo), const LOG_SYSLOG Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_USER = 8

pkg log/syslog (netbsd-arm64-cgo), const LOG_USER Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_UUCP = 64

pkg log/syslog (netbsd-arm64-cgo), const LOG_UUCP Priority

pkg log/syslog (netbsd-arm64-cgo), const LOG_WARNING = 4

pkg log/syslog (netbsd-arm64-cgo), const LOG_WARNING Priority

pkg log/syslog (netbsd-arm64-cgo), func Dial(string, string, Priority, string) (*Writer, error)

pkg log/syslog (netbsd-arm64-cgo), func NewLogger(Priority, int) (*log.Logger, error)

pkg log/syslog (netbsd-arm64-cgo), func New(Priority, string) (*Writer, error)

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Alert(string) error

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Close() error

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Crit(string) error

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Debug(string) error

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Emerg(string) error

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Err(string) error

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Info(string) error

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Notice(string) error

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Warning(string) error

pkg log/syslog (netbsd-arm64-cgo), method (*Writer) Write([]uint8) (int, error)

pkg log/syslog (netbsd-arm64-cgo), type Priority int

pkg log/syslog (netbsd-arm64-cgo), type Writer struct

pkg log/syslog (netbsd-arm64), const LOG_ALERT = 1

pkg log/syslog (netbsd-arm64), const LOG_ALERT Priority

pkg log/syslog (netbsd-arm64), const LOG_AUTH = 32

pkg log/syslog (netbsd-arm64), const LOG_AUTH Priority

pkg log/syslog (netbsd-arm64), const LOG_AUTHPRIV = 80

pkg log/syslog (netbsd-arm64), const LOG_AUTHPRIV Priority

pkg log/syslog (netbsd-arm64), const LOG_CRIT = 2

pkg log/syslog (netbsd-arm64), const LOG_CRIT Priority

pkg log/syslog (netbsd-arm64), const LOG_CRON = 72

pkg log/syslog (netbsd-arm64), const LOG_CRON Priority

pkg log/syslog (netbsd-arm64), const LOG_DAEMON = 24

pkg log/syslog (netbsd-arm64), const LOG_DAEMON Priority

pkg log/syslog (netbsd-arm64), const LOG_DEBUG = 7

pkg log/syslog (netbsd-arm64), const LOG_DEBUG Priority

pkg log/syslog (netbsd-arm64), const LOG_EMERG = 0

pkg log/syslog (netbsd-arm64), const LOG_EMERG Priority

pkg log/syslog (netbsd-arm64), const LOG_ERR = 3

pkg log/syslog (netbsd-arm64), const LOG_ERR Priority

pkg log/syslog (netbsd-arm64), const LOG_FTP = 88

pkg log/syslog (netbsd-arm64), const LOG_FTP Priority

pkg log/syslog (netbsd-arm64), const LOG_INFO = 6

pkg log/syslog (netbsd-arm64), const LOG_INFO Priority

pkg log/syslog (netbsd-arm64), const LOG_KERN = 0

pkg log/syslog (netbsd-arm64), const LOG_KERN Priority

pkg log/syslog (netbsd-arm64), const LOG_LOCAL0 = 128

pkg log/syslog (netbsd-arm64), const LOG_LOCAL0 Priority

pkg log/syslog (netbsd-arm64), const LOG_LOCAL1 = 136

pkg log/syslog (netbsd-arm64), const LOG_LOCAL1 Priority

pkg log/syslog (netbsd-arm64), const LOG_LOCAL2 = 144

pkg log/syslog (netbsd-arm64), const LOG_LOCAL2 Priority

pkg log/syslog (netbsd-arm64), const LOG_LOCAL3 = 152

pkg log/syslog (netbsd-arm64), const LOG_LOCAL3 Priority

pkg log/syslog (netbsd-arm64), const LOG_LOCAL4 = 160

pkg log/syslog (netbsd-arm64), const LOG_LOCAL4 Priority

pkg log/syslog (netbsd-arm64), const LOG_LOCAL5 = 168

pkg log/syslog (netbsd-arm64), const LOG_LOCAL5 Priority

pkg log/syslog (netbsd-arm64), const LOG_LOCAL6 = 176

pkg log/syslog (netbsd-arm64), const LOG_LOCAL6 Priority

pkg log/syslog (netbsd-arm64), const LOG_LOCAL7 = 184

pkg log/syslog (netbsd-arm64), const LOG_LOCAL7 Priority

pkg log/syslog (netbsd-arm64), const LOG_LPR = 48

pkg log/syslog (netbsd-arm64), const LOG_LPR Priority

pkg log/syslog (netbsd-arm64), const LOG_MAIL = 16

pkg log/syslog (netbsd-arm64), const LOG_MAIL Priority

pkg log/syslog (netbsd-arm64), const LOG_NEWS = 56

pkg log/syslog (netbsd-arm64), const LOG_NEWS Priority

pkg log/syslog (netbsd-arm64), const LOG_NOTICE = 5

pkg log/syslog (netbsd-arm64), const LOG_NOTICE Priority

pkg log/syslog (netbsd-arm64), const LOG_SYSLOG = 40

pkg log/syslog (netbsd-arm64), const LOG_SYSLOG Priority

pkg log/syslog (netbsd-arm64), const LOG_USER = 8

pkg log/syslog (netbsd-arm64), const LOG_USER Priority

pkg log/syslog (netbsd-arm64), const LOG_UUCP = 64

pkg log/syslog (netbsd-arm64), const LOG_UUCP Priority

pkg log/syslog (netbsd-arm64), const LOG_WARNING = 4

pkg log/syslog (netbsd-arm64), const LOG_WARNING Priority

pkg log/syslog (netbsd-arm64), func Dial(string, string, Priority, string) (*Writer, error)

pkg log/syslog (netbsd-arm64), func NewLogger(Priority, int) (*log.Logger, error)

pkg log/syslog (netbsd-arm64), func New(Priority, string) (*Writer, error)

pkg log/syslog (netbsd-arm64), method (*Writer) Alert(string) error

pkg log/syslog (netbsd-arm64), method (*Writer) Close() error

pkg log/syslog (netbsd-arm64), method (*Writer) Crit(string) error

pkg log/syslog (netbsd-arm64), method (*Writer) Debug(string) error

pkg log/syslog (netbsd-arm64), method (*Writer) Emerg(string) error

pkg log/syslog (netbsd-arm64), method (*Writer) Err(string) error

pkg log/syslog (netbsd-arm64), method (*Writer) Info(string) error

pkg log/syslog (netbsd-arm64), method (*Writer) Notice(string) error

pkg log/syslog (netbsd-arm64), method (*Writer) Warning(string) error

pkg log/syslog (netbsd-arm64), method (*Writer) Write([]uint8) (int, error)

pkg log/syslog (netbsd-arm64), type Priority int

pkg log/syslog (netbsd-arm64), type Writer struct

pkg math/big, method (*Int) TrailingZeroBits() uint

pkg math/big, method (*Rat) SetUint64(uint64) *Rat

pkg net/http, const SameSiteNoneMode = 4

pkg net/http, const SameSiteNoneMode SameSite

pkg net/http, const StatusEarlyHints = 103

pkg net/http, const StatusEarlyHints ideal-int

pkg net/http, func NewRequestWithContext(context.Context, string, string, io.Reader) (*Request, error)

pkg net/http, method (Header) Clone() Header

pkg net/http, method (*Request) Clone(context.Context) *Request

pkg net/http, method (*Transport) Clone() *Transport

pkg net/http, type Server struct, BaseContext func(net.Listener) context.Context

pkg net/http, type Server struct, ConnContext func(context.Context, net.Conn) context.Context

pkg net/http, type Transport struct, ForceAttemptHTTP2 bool

pkg net/http, type Transport struct, ReadBufferSize int

pkg net/http, type Transport struct, WriteBufferSize int

pkg net, method (*DNSConfigError) Unwrap() error

pkg net, method (*OpError) Unwrap() error

pkg net, type DNSError struct, IsNotFound bool

pkg net, type ListenConfig struct, KeepAlive time.Duration

pkg net/url, method (*Error) Unwrap() error

pkg os/exec, method (*Cmd) String() string

pkg os/exec, method (*Error) Unwrap() error

pkg os, func UserConfigDir() (string, error)

pkg os, method (*LinkError) Unwrap() error

pkg os, method (*PathError) Unwrap() error

pkg os, method (*SyscallError) Unwrap() error

pkg os (netbsd-arm64-cgo), const DevNull = "/dev/null"

pkg os (netbsd-arm64-cgo), const O_APPEND = 8

pkg os (netbsd-arm64-cgo), const O_CREATE = 512

pkg os (netbsd-arm64-cgo), const O_EXCL = 2048

pkg os (netbsd-arm64-cgo), const O_SYNC = 128

pkg os (netbsd-arm64-cgo), const O_TRUNC = 1024

pkg os (netbsd-arm64-cgo), const PathListSeparator = 58

pkg os (netbsd-arm64-cgo), const PathSeparator = 47

pkg os (netbsd-arm64), const DevNull = "/dev/null"

pkg os (netbsd-arm64), const O_APPEND = 8

pkg os (netbsd-arm64), const O_CREATE = 512

pkg os (netbsd-arm64), const O_EXCL = 2048

pkg os (netbsd-arm64), const O_SYNC = 128

pkg os (netbsd-arm64), const O_TRUNC = 1024

pkg os (netbsd-arm64), const PathListSeparator = 58

pkg os (netbsd-arm64), const PathSeparator = 47

pkg path/filepath (netbsd-arm64-cgo), const ListSeparator = 58

pkg path/filepath (netbsd-arm64-cgo), const Separator = 47

pkg path/filepath (netbsd-arm64), const ListSeparator = 58

pkg path/filepath (netbsd-arm64), const Separator = 47

pkg reflect, method (Value) IsZero() bool

pkg runtime (netbsd-arm64-cgo), const GOARCH = "arm64"

pkg runtime (netbsd-arm64-cgo), const GOOS = "netbsd"

pkg runtime (netbsd-arm64), const GOARCH = "arm64"

pkg runtime (netbsd-arm64), const GOOS = "netbsd"

pkg strings, func ToValidUTF8(string, string) string

pkg syscall, method (Errno) Is(error) bool

pkg syscall (netbsd-arm64-cgo), const AF_APPLETALK = 16

pkg syscall (netbsd-arm64-cgo), const AF_APPLETALK ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_ARP = 28

pkg syscall (netbsd-arm64-cgo), const AF_ARP ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_BLUETOOTH = 31

pkg syscall (netbsd-arm64-cgo), const AF_BLUETOOTH ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_CCITT = 10

pkg syscall (netbsd-arm64-cgo), const AF_CCITT ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_CHAOS = 5

pkg syscall (netbsd-arm64-cgo), const AF_CHAOS ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_CNT = 21

pkg syscall (netbsd-arm64-cgo), const AF_CNT ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_COIP = 20

pkg syscall (netbsd-arm64-cgo), const AF_COIP ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_DATAKIT = 9

pkg syscall (netbsd-arm64-cgo), const AF_DATAKIT ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_DECnet = 12

pkg syscall (netbsd-arm64-cgo), const AF_DECnet ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_DLI = 13

pkg syscall (netbsd-arm64-cgo), const AF_DLI ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_E164 = 26

pkg syscall (netbsd-arm64-cgo), const AF_E164 ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_ECMA = 8

pkg syscall (netbsd-arm64-cgo), const AF_ECMA ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_HYLINK = 15

pkg syscall (netbsd-arm64-cgo), const AF_HYLINK ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_IEEE80211 = 32

pkg syscall (netbsd-arm64-cgo), const AF_IEEE80211 ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_IMPLINK = 3

pkg syscall (netbsd-arm64-cgo), const AF_IMPLINK ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_INET6 = 24

pkg syscall (netbsd-arm64-cgo), const AF_IPX = 23

pkg syscall (netbsd-arm64-cgo), const AF_IPX ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_ISDN = 26

pkg syscall (netbsd-arm64-cgo), const AF_ISDN ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_ISO = 7

pkg syscall (netbsd-arm64-cgo), const AF_ISO ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_LAT = 14

pkg syscall (netbsd-arm64-cgo), const AF_LAT ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_LINK = 18

pkg syscall (netbsd-arm64-cgo), const AF_LINK ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_LOCAL = 1

pkg syscall (netbsd-arm64-cgo), const AF_LOCAL ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_MAX = 35

pkg syscall (netbsd-arm64-cgo), const AF_MAX ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_MPLS = 33

pkg syscall (netbsd-arm64-cgo), const AF_MPLS ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_NATM = 27

pkg syscall (netbsd-arm64-cgo), const AF_NATM ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_NS = 6

pkg syscall (netbsd-arm64-cgo), const AF_NS ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_OROUTE = 17

pkg syscall (netbsd-arm64-cgo), const AF_OROUTE ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_OSI = 7

pkg syscall (netbsd-arm64-cgo), const AF_OSI ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_PUP = 4

pkg syscall (netbsd-arm64-cgo), const AF_PUP ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_ROUTE = 34

pkg syscall (netbsd-arm64-cgo), const AF_ROUTE ideal-int

pkg syscall (netbsd-arm64-cgo), const AF_SNA = 11

pkg syscall (netbsd-arm64-cgo), const AF_SNA ideal-int

pkg syscall (netbsd-arm64-cgo), const ARPHRD_ARCNET = 7

pkg syscall (netbsd-arm64-cgo), const ARPHRD_ARCNET ideal-int

pkg syscall (netbsd-arm64-cgo), const ARPHRD_ETHER = 1

pkg syscall (netbsd-arm64-cgo), const ARPHRD_ETHER ideal-int

pkg syscall (netbsd-arm64-cgo), const ARPHRD_FRELAY = 15

pkg syscall (netbsd-arm64-cgo), const ARPHRD_FRELAY ideal-int

pkg syscall (netbsd-arm64-cgo), const ARPHRD_IEEE1394 = 24

pkg syscall (netbsd-arm64-cgo), const ARPHRD_IEEE1394 ideal-int

pkg syscall (netbsd-arm64-cgo), const ARPHRD_IEEE802 = 6

pkg syscall (netbsd-arm64-cgo), const ARPHRD_IEEE802 ideal-int

pkg syscall (netbsd-arm64-cgo), const ARPHRD_STRIP = 23

pkg syscall (netbsd-arm64-cgo), const ARPHRD_STRIP ideal-int

pkg syscall (netbsd-arm64-cgo), const B0 = 0

pkg syscall (netbsd-arm64-cgo), const B0 ideal-int

pkg syscall (netbsd-arm64-cgo), const B110 = 110

pkg syscall (netbsd-arm64-cgo), const B110 ideal-int

pkg syscall (netbsd-arm64-cgo), const B115200 = 115200

pkg syscall (netbsd-arm64-cgo), const B115200 ideal-int

pkg syscall (netbsd-arm64-cgo), const B1200 = 1200

pkg syscall (netbsd-arm64-cgo), const B1200 ideal-int

pkg syscall (netbsd-arm64-cgo), const B134 = 134

pkg syscall (netbsd-arm64-cgo), const B134 ideal-int

pkg syscall (netbsd-arm64-cgo), const B14400 = 14400

pkg syscall (netbsd-arm64-cgo), const B14400 ideal-int

pkg syscall (netbsd-arm64-cgo), const B150 = 150

pkg syscall (netbsd-arm64-cgo), const B150 ideal-int

pkg syscall (netbsd-arm64-cgo), const B1800 = 1800

pkg syscall (netbsd-arm64-cgo), const B1800 ideal-int

pkg syscall (netbsd-arm64-cgo), const B19200 = 19200

pkg syscall (netbsd-arm64-cgo), const B19200 ideal-int

pkg syscall (netbsd-arm64-cgo), const B200 = 200

pkg syscall (netbsd-arm64-cgo), const B200 ideal-int

pkg syscall (netbsd-arm64-cgo), const B230400 = 230400

pkg syscall (netbsd-arm64-cgo), const B230400 ideal-int

pkg syscall (netbsd-arm64-cgo), const B2400 = 2400

pkg syscall (netbsd-arm64-cgo), const B2400 ideal-int

pkg syscall (netbsd-arm64-cgo), const B28800 = 28800

pkg syscall (netbsd-arm64-cgo), const B28800 ideal-int

pkg syscall (netbsd-arm64-cgo), const B300 = 300

pkg syscall (netbsd-arm64-cgo), const B300 ideal-int

pkg syscall (netbsd-arm64-cgo), const B38400 = 38400

pkg syscall (netbsd-arm64-cgo), const B38400 ideal-int

pkg syscall (netbsd-arm64-cgo), const B460800 = 460800

pkg syscall (netbsd-arm64-cgo), const B460800 ideal-int

pkg syscall (netbsd-arm64-cgo), const B4800 = 4800

pkg syscall (netbsd-arm64-cgo), const B4800 ideal-int

pkg syscall (netbsd-arm64-cgo), const B50 = 50

pkg syscall (netbsd-arm64-cgo), const B50 ideal-int

pkg syscall (netbsd-arm64-cgo), const B57600 = 57600

pkg syscall (netbsd-arm64-cgo), const B57600 ideal-int

pkg syscall (netbsd-arm64-cgo), const B600 = 600

pkg syscall (netbsd-arm64-cgo), const B600 ideal-int

pkg syscall (netbsd-arm64-cgo), const B7200 = 7200

pkg syscall (netbsd-arm64-cgo), const B7200 ideal-int

pkg syscall (netbsd-arm64-cgo), const B75 = 75

pkg syscall (netbsd-arm64-cgo), const B75 ideal-int

pkg syscall (netbsd-arm64-cgo), const B76800 = 76800

pkg syscall (netbsd-arm64-cgo), const B76800 ideal-int

pkg syscall (netbsd-arm64-cgo), const B921600 = 921600

pkg syscall (netbsd-arm64-cgo), const B921600 ideal-int

pkg syscall (netbsd-arm64-cgo), const B9600 = 9600

pkg syscall (netbsd-arm64-cgo), const B9600 ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCFEEDBACK = 2147762813

pkg syscall (netbsd-arm64-cgo), const BIOCFEEDBACK ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCFLUSH = 536887912

pkg syscall (netbsd-arm64-cgo), const BIOCFLUSH ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGBLEN = 1074020966

pkg syscall (netbsd-arm64-cgo), const BIOCGBLEN ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGDLT = 1074020970

pkg syscall (netbsd-arm64-cgo), const BIOCGDLT ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGDLTLIST = 3222291063

pkg syscall (netbsd-arm64-cgo), const BIOCGDLTLIST ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGETIF = 1083196011

pkg syscall (netbsd-arm64-cgo), const BIOCGETIF ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGFEEDBACK = 1074020988

pkg syscall (netbsd-arm64-cgo), const BIOCGFEEDBACK ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGHDRCMPLT = 1074020980

pkg syscall (netbsd-arm64-cgo), const BIOCGHDRCMPLT ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGRTIMEOUT = 1074807419

pkg syscall (netbsd-arm64-cgo), const BIOCGRTIMEOUT ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGSEESENT = 1074020984

pkg syscall (netbsd-arm64-cgo), const BIOCGSEESENT ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGSTATS = 1082147439

pkg syscall (netbsd-arm64-cgo), const BIOCGSTATS ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCGSTATSOLD = 1074283119

pkg syscall (netbsd-arm64-cgo), const BIOCGSTATSOLD ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCIMMEDIATE = 2147762800

pkg syscall (netbsd-arm64-cgo), const BIOCIMMEDIATE ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCPROMISC = 536887913

pkg syscall (netbsd-arm64-cgo), const BIOCPROMISC ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSBLEN = 3221504614

pkg syscall (netbsd-arm64-cgo), const BIOCSBLEN ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSDLT = 2147762806

pkg syscall (netbsd-arm64-cgo), const BIOCSDLT ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSETF = 2148549223

pkg syscall (netbsd-arm64-cgo), const BIOCSETF ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSETIF = 2156937836

pkg syscall (netbsd-arm64-cgo), const BIOCSETIF ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSFEEDBACK = 2147762813

pkg syscall (netbsd-arm64-cgo), const BIOCSFEEDBACK ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSHDRCMPLT = 2147762805

pkg syscall (netbsd-arm64-cgo), const BIOCSHDRCMPLT ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSRTIMEOUT = 2148549242

pkg syscall (netbsd-arm64-cgo), const BIOCSRTIMEOUT ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSSEESENT = 2147762809

pkg syscall (netbsd-arm64-cgo), const BIOCSSEESENT ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSTCPF = 2148549234

pkg syscall (netbsd-arm64-cgo), const BIOCSTCPF ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCSUDPF = 2148549235

pkg syscall (netbsd-arm64-cgo), const BIOCSUDPF ideal-int

pkg syscall (netbsd-arm64-cgo), const BIOCVERSION = 1074020977

pkg syscall (netbsd-arm64-cgo), const BIOCVERSION ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_A = 16

pkg syscall (netbsd-arm64-cgo), const BPF_ABS = 32

pkg syscall (netbsd-arm64-cgo), const BPF_ABS ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_ADD = 0

pkg syscall (netbsd-arm64-cgo), const BPF_ADD ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_A ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_ALIGNMENT32 = 4

pkg syscall (netbsd-arm64-cgo), const BPF_ALIGNMENT32 ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_ALIGNMENT = 8

pkg syscall (netbsd-arm64-cgo), const BPF_ALIGNMENT ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_ALU = 4

pkg syscall (netbsd-arm64-cgo), const BPF_ALU ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_AND = 80

pkg syscall (netbsd-arm64-cgo), const BPF_AND ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_B = 16

pkg syscall (netbsd-arm64-cgo), const BPF_B ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_DFLTBUFSIZE = 1048576

pkg syscall (netbsd-arm64-cgo), const BPF_DFLTBUFSIZE ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_DIV = 48

pkg syscall (netbsd-arm64-cgo), const BPF_DIV ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_H = 8

pkg syscall (netbsd-arm64-cgo), const BPF_H ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_IMM = 0

pkg syscall (netbsd-arm64-cgo), const BPF_IMM ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_IND = 64

pkg syscall (netbsd-arm64-cgo), const BPF_IND ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_JA = 0

pkg syscall (netbsd-arm64-cgo), const BPF_JA ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_JEQ = 16

pkg syscall (netbsd-arm64-cgo), const BPF_JEQ ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_JGE = 48

pkg syscall (netbsd-arm64-cgo), const BPF_JGE ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_JGT = 32

pkg syscall (netbsd-arm64-cgo), const BPF_JGT ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_JMP = 5

pkg syscall (netbsd-arm64-cgo), const BPF_JMP ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_JSET = 64

pkg syscall (netbsd-arm64-cgo), const BPF_JSET ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_K = 0

pkg syscall (netbsd-arm64-cgo), const BPF_K ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_LD = 0

pkg syscall (netbsd-arm64-cgo), const BPF_LD ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_LDX = 1

pkg syscall (netbsd-arm64-cgo), const BPF_LDX ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_LEN = 128

pkg syscall (netbsd-arm64-cgo), const BPF_LEN ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_LSH = 96

pkg syscall (netbsd-arm64-cgo), const BPF_LSH ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MAJOR_VERSION = 1

pkg syscall (netbsd-arm64-cgo), const BPF_MAJOR_VERSION ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MAXBUFSIZE = 16777216

pkg syscall (netbsd-arm64-cgo), const BPF_MAXBUFSIZE ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MAXINSNS = 512

pkg syscall (netbsd-arm64-cgo), const BPF_MAXINSNS ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MEM = 96

pkg syscall (netbsd-arm64-cgo), const BPF_MEM ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MEMWORDS = 16

pkg syscall (netbsd-arm64-cgo), const BPF_MEMWORDS ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MINBUFSIZE = 32

pkg syscall (netbsd-arm64-cgo), const BPF_MINBUFSIZE ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MINOR_VERSION = 1

pkg syscall (netbsd-arm64-cgo), const BPF_MINOR_VERSION ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MISC = 7

pkg syscall (netbsd-arm64-cgo), const BPF_MISC ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MSH = 160

pkg syscall (netbsd-arm64-cgo), const BPF_MSH ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_MUL = 32

pkg syscall (netbsd-arm64-cgo), const BPF_MUL ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_NEG = 128

pkg syscall (netbsd-arm64-cgo), const BPF_NEG ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_OR = 64

pkg syscall (netbsd-arm64-cgo), const BPF_OR ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_RELEASE = 199606

pkg syscall (netbsd-arm64-cgo), const BPF_RELEASE ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_RET = 6

pkg syscall (netbsd-arm64-cgo), const BPF_RET ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_RSH = 112

pkg syscall (netbsd-arm64-cgo), const BPF_RSH ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_ST = 2

pkg syscall (netbsd-arm64-cgo), const BPF_ST ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_STX = 3

pkg syscall (netbsd-arm64-cgo), const BPF_STX ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_SUB = 16

pkg syscall (netbsd-arm64-cgo), const BPF_SUB ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_TAX = 0

pkg syscall (netbsd-arm64-cgo), const BPF_TAX ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_TXA = 128

pkg syscall (netbsd-arm64-cgo), const BPF_TXA ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_W = 0

pkg syscall (netbsd-arm64-cgo), const BPF_W ideal-int

pkg syscall (netbsd-arm64-cgo), const BPF_X = 8

pkg syscall (netbsd-arm64-cgo), const BPF_X ideal-int

pkg syscall (netbsd-arm64-cgo), const BRKINT = 2

pkg syscall (netbsd-arm64-cgo), const BRKINT ideal-int

pkg syscall (netbsd-arm64-cgo), const CFLUSH = 15

pkg syscall (netbsd-arm64-cgo), const CFLUSH ideal-int

pkg syscall (netbsd-arm64-cgo), const CLOCAL = 32768

pkg syscall (netbsd-arm64-cgo), const CLOCAL ideal-int

pkg syscall (netbsd-arm64-cgo), const CLONE_CSIGNAL = 255

pkg syscall (netbsd-arm64-cgo), const CLONE_CSIGNAL ideal-int

pkg syscall (netbsd-arm64-cgo), const CLONE_FILES = 1024

pkg syscall (netbsd-arm64-cgo), const CLONE_FILES ideal-int

pkg syscall (netbsd-arm64-cgo), const CLONE_FS = 512

pkg syscall (netbsd-arm64-cgo), const CLONE_FS ideal-int

pkg syscall (netbsd-arm64-cgo), const CLONE_PID = 4096

pkg syscall (netbsd-arm64-cgo), const CLONE_PID ideal-int

pkg syscall (netbsd-arm64-cgo), const CLONE_PTRACE = 8192

pkg syscall (netbsd-arm64-cgo), const CLONE_PTRACE ideal-int

pkg syscall (netbsd-arm64-cgo), const CLONE_SIGHAND = 2048

pkg syscall (netbsd-arm64-cgo), const CLONE_SIGHAND ideal-int

pkg syscall (netbsd-arm64-cgo), const CLONE_VFORK = 16384

pkg syscall (netbsd-arm64-cgo), const CLONE_VFORK ideal-int

pkg syscall (netbsd-arm64-cgo), const CLONE_VM = 256

pkg syscall (netbsd-arm64-cgo), const CLONE_VM ideal-int

pkg syscall (netbsd-arm64-cgo), const CREAD = 2048

pkg syscall (netbsd-arm64-cgo), const CREAD ideal-int

pkg syscall (netbsd-arm64-cgo), const CS5 = 0

pkg syscall (netbsd-arm64-cgo), const CS5 ideal-int

pkg syscall (netbsd-arm64-cgo), const CS6 = 256

pkg syscall (netbsd-arm64-cgo), const CS6 ideal-int

pkg syscall (netbsd-arm64-cgo), const CS7 = 512

pkg syscall (netbsd-arm64-cgo), const CS7 ideal-int

pkg syscall (netbsd-arm64-cgo), const CS8 = 768

pkg syscall (netbsd-arm64-cgo), const CS8 ideal-int

pkg syscall (netbsd-arm64-cgo), const CSIZE = 768

pkg syscall (netbsd-arm64-cgo), const CSIZE ideal-int

pkg syscall (netbsd-arm64-cgo), const CSTART = 17

pkg syscall (netbsd-arm64-cgo), const CSTART ideal-int

pkg syscall (netbsd-arm64-cgo), const CSTATUS = 20

pkg syscall (netbsd-arm64-cgo), const CSTATUS ideal-int

pkg syscall (netbsd-arm64-cgo), const CSTOP = 19

pkg syscall (netbsd-arm64-cgo), const CSTOPB = 1024

pkg syscall (netbsd-arm64-cgo), const CSTOPB ideal-int

pkg syscall (netbsd-arm64-cgo), const CSTOP ideal-int

pkg syscall (netbsd-arm64-cgo), const CSUSP = 26

pkg syscall (netbsd-arm64-cgo), const CSUSP ideal-int

pkg syscall (netbsd-arm64-cgo), const CTL_MAXNAME = 12

pkg syscall (netbsd-arm64-cgo), const CTL_MAXNAME ideal-int

pkg syscall (netbsd-arm64-cgo), const CTL_NET = 4

pkg syscall (netbsd-arm64-cgo), const CTL_NET ideal-int

pkg syscall (netbsd-arm64-cgo), const CTL_QUERY = -2

pkg syscall (netbsd-arm64-cgo), const CTL_QUERY ideal-int

pkg syscall (netbsd-arm64-cgo), const DIOCBSFLUSH = 536896632

pkg syscall (netbsd-arm64-cgo), const DIOCBSFLUSH ideal-int

pkg syscall (netbsd-arm64-cgo), const DLT_A429 = 184

pkg syscall (netbsd-arm64-cgo), const DLT_A429 ideal-int

pkg syscall (netbsd-arm64-cgo), const DLT_A653_ICM = 185

pkg syscall (netbsd-arm64-cgo), const DLT_A653_ICM ideal-int

pkg syscall (netbsd-arm64-cgo), const DLT_AIRONET_HEADER = 120

pkg syscall (netbsd-arm64-cgo), const DLT_AIRONET_HEADER ideal-int

pkg syscall (netbsd-arm64-cgo), const DLT_AOS =

Ed25519 Constants and Types
crypto/ed25519.PrivateKeySize: 64 — Size in bytes for Ed25519 private keys.

crypto/ed25519.PrivateKeySize ideal-int: 64 — Ideal integer value for the constant.

crypto/ed25519.PublicKeySize: 32 — Ed25519 public keys are 32 bytes.

crypto/ed25519.PublicKeySize ideal-int: 32 — Ideal integer.

crypto/ed25519.SeedSize: 32 — Seeds for key generation are 32 bytes.

crypto/ed25519.SeedSize ideal-int: 32 — Ideal integer.

crypto/ed25519.SignatureSize: 64 — Ed25519 signatures are 64 bytes.

crypto/ed25519.SignatureSize ideal-int: 64 — Ideal integer.

crypto/ed25519.GenerateKey(io.Reader) (PublicKey, PrivateKey, error): Generates a new public/private Ed25519 key pair.

crypto/ed25519.NewKeyFromSeed([]uint8) PrivateKey: Returns a private key deterministically from a 32-byte seed.

crypto/ed25519.Sign(PrivateKey, []uint8) []uint8: Signs a message with a private key.

crypto/ed25519.Verify(PublicKey, []uint8, []uint8) bool: Verifies a signature with a public key.

crypto/ed25519.PrivateKey: type []uint8 — Underlying type is a byte slice.

crypto/ed25519.PublicKey: type []uint8 — Underlying type is a byte slice.

crypto/ed25519.(PrivateKey) Public() crypto.PublicKey: Returns the public key corresponding to a private key.

crypto/ed25519.(PrivateKey) Seed() []uint8: Returns the seed from which a private key was derived.

crypto/ed25519.(PrivateKey) Sign(io.Reader, []uint8, crypto.SignerOpts) ([]uint8, error): Signs a message (implements crypto.Signer).

TLS and X.509 Ed25519 Integration
crypto/tls.Ed25519: 2055 — Ed25519 signature scheme ID used in TLS.

crypto/tls.Ed25519 SignatureScheme: Enum for signature schemes, Ed25519 value is 2055.

crypto/x509.Ed25519: 4 — PublicKeyAlgorithm for Ed25519.

crypto/x509.Ed25519 PublicKeyAlgorithm: Ed25519 as a recognized public key algorithm.

crypto/x509.PureEd25519: 16 — SignatureAlgorithm for Pure Ed25519 (no hashing).

crypto/x509.PureEd25519 SignatureAlgorithm: Value for pure (non-hashed) Ed25519 in x509.

Other API Details
pkg bytes, func ToValidUTF8([]uint8, []uint8) []uint8: Makes a valid UTF-8 version of a byte slice, replacing invalid sequences.

pkg strings, func ToValidUTF8(string, string) string: Same as above, but for strings.

Key Structures in database/sql
database/sql.NullInt32, database/sql.NullTime: Struct types for nullable int32 and time columns from SQL.

Scan() error, Value() (driver.Value, error): Methods for scanning from SQL and converting to DB values.

Fields for NullInt32: Int32 int32, Valid bool.

Fields for NullTime: Time time.Time, Valid bool.

Symbol and Error Types
debug/elf.Symbol: Library string, Version string: ELF symbol structure fields for shared library + symbol versioning.

debug/dwarf.UnsupportedType: Struct for unsupported DWARF types.

debug/dwarf.(*UnsupportedType) Common(), Size(), String(): Methods for type info, size, and string representation.

encoding/csv.(*ParseError).Unwrap(), encoding/json.(*MarshalerError).Unwrap(): Unwraps the cause of parsing or marshaling errors.

Error Utilities
errors.As(error, interface{}) bool, errors.Is(error, error) bool, errors.Unwrap(error) error: Helper functions for error chains.

These are the primary properties and meanings for the given Go runtime/package constants, functions, types, and methods, especially focusing on Ed25519 cryptography constants and APIs, and nullable SQL helpers.

References
: Ed25519 constants, types, and functions in crypto/ed25519.

: Ed25519 in crypto/tls.

: Ed25519 and PureEd25519 in crypto/x509.

: ToValidUTF8 in bytes, strings package.

: database/sql nullable types and methods.

: ELF symbol Library and Version fields.

: DWARF unsupported type methods.

: CSV/JSON error unwrap and error utilities.
