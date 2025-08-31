package bithub.availability

# Input shape:
# { continuity: {...}, platform: {...} }

deny[msg] {
  count(input.continuity.mirrors) == 0
  msg := "No mirrors configured in .bit/continuity.json"
}
deny[msg] {
  count(input.continuity.orchestrators) == 0
  msg := "No orchestrators configured"
}
deny[msg] {
  count(input.continuity.registries) == 0
  msg := "No registries configured"
}

warn[msg] {
  some i
  m := input.continuity.mirrors[i]
  not m.write
  msg := sprintf("Mirror '%s' is read-only", [m.name])
}

warn[msg] {
  input.platform.github.rate_limit.remaining < 200
  msg := "GitHub API rate limit low; expect throttling"
}

warn[msg] {
  input.platform.queue.inflight > input.continuity.policies.max_concurrent_ci
  msg := sprintf("Inflight workflows (%d) exceed configured maximum (%d)", [input.platform.queue.inflight, input.continuity.policies.max_concurrent_ci])
}
