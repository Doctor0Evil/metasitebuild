package alnfantasia.decompression

deny[msg] {
  input.decompress.method == "tar"
  not startswith(input.decompress.output, "/tmp/")
  msg := "tar decompression only allowed in /tmp/"
}

deny[msg] {
  input.decompress.method == "any"
  input.decompress.file_size_mb > 120
  msg := sprintf("Decompression file %q (%d MB) too large for runner railguard", [input.decompress.input, input.decompress.file_size_mb])
}

allow {
  not deny[_]
}
