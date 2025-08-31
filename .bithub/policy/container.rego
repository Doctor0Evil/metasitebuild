package bithub.container

# Policy input shape:
# { kind: "container_image", name: "ghcr.io/owner/image:tag", metadata: <docker inspect json> }

# Required OCI labels
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

# Discourage :latest in strict/paranoid by emitting a warning.
# Enforcement happens in the workflow's threshold logic.
warn[msg] {
  endswith(lower(input.name), ":latest")
  msg := "latest tag used; prefer immutable version tags"
}

# Non-root recommended (warn); can be tightened by threshold
warn[msg] {
  user := image_user()
  user == "" or user == "0" or user == "root"
  msg := "Container user is root or unspecified; prefer non-root"
}

# Helper: read OCI label from docker inspect
oci_label(key) {
  some i
  img := input.metadata[i]
  labels := img.Config.Labels
  labels[key]
}

# Helper: read User field
image_user() = user {
  some i
  img := input.metadata[i]
  user := img.Config.User
}
