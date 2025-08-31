package bithub.container

deny[msg] {
  input.kind == "container_image"
  not oci_label("org.opencontainers.image.source")
  msg := "Missing OCI label: org.opencontainers.image.source"
}

deny[msg] {
  input.kind == "container_image"
  not oci_label("org.opencontainers.image.description")
  msg := "Missing OCI label: org.opencontainers.image.description"
}

deny[msg] {
  input.kind == "container_image"
  not oci_label("org.opencontainers.image.licenses")
  msg := "Missing OCI label: org.opencontainers.image.licenses"
}

warn[msg] {
  endswith(lower(input.name), ":latest")
  msg := "Avoid using ':latest' tag for container images"
}

warn[msg] {
  user := image_user()
  user == "" or user == "0" or user == "root"
  msg := "Container image runs as root user; prefer non-root"
}

oci_label(key) {
  some i
  img := input.metadata[i]
  labels := img.Config.Labels
  labels[key]
}

image_user() = user {
  some i
  img := input.metadata[i]
  user := img.Config.User
}
