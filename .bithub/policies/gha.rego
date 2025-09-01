package bithub.gha

default allow = false

deny[msg] {
  input.runs_on != "self-hosted"
  input.runs_on != "ubuntu-lts-mirrored"
  msg := sprintf("runs-on not permitted: %v", [input.runs_on])
}

deny[msg] {
  input.workflow_uses_unpinned_actions == true
  msg := "All actions must be pinned by sha256 digest"
}

deny[msg] {
  not input.org == "Doctor0Evil"
  not input.org == "BitHubOrg"
  msg := sprintf("Unauthorized org: %v", [input.org])
}

allow {
  not deny[_]
}
