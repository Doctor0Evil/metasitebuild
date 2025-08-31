package bithub.decompression

default allow = false

allow {
  input.action == "decompress"
  input.method in {"zlib", "gzip", "tar"}
  input.size_mb <= 128
  not input.repo in blacklisted_repos
}
blacklisted_repos := {"evilcorp/payloads"}
deny[msg] {
  not allow
  msg := sprintf("Denied decompression (repo: %v, method: %v, size: %d)", [input.repo, input.method, input.size_mb])
}
