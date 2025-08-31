package bithub.availability

deny[msg] {
  count(input.continuity.mirrors) == 0
  msg := "No mirrors configured in continuity manifest"
}

deny[msg] {
  count(input.continuity.orchestrators) == 0
  msg := "No orchestrators configured in continuity manifest"
}

deny[msg] {
  count(input.continuity.registries) == 0
  msg := "No registries configured in continuity manifest"
}

warn[msg] {
  some m
  m := input.continuity.mirrors[_]
  not m.write
  msg := sprintf("Mirror '%s' is read-only", [m.name])
}

warn[msg] {
  input.platform.github.rate_limit.remaining < 200
  msg := "GitHub API rate limit is low"
}

warn[msg] {
  input.platform.queue.inflight > input.continuity.policies.max_concurrent_ci
  msg := sprintf("Inflight workflows (%d) exceed maximum (%d)", [input.platform.queue.inflight, input.continuity.policies.max_concurrent_ci])
}
