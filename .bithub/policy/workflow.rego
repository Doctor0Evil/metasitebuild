package bithub.workflow

# Enforce workflow hygiene: placement, permissions, concurrency, action pinning,
# adaptive runs-on, and timeouts. Deny unsafe patterns, warn on drift.

deny[msg] {
  input.path
  not startswith(input.path, ".github/workflows/")
  msg := sprintf("Workflow outside .github/workflows: %s", [input.path])
}

deny[msg] {
  not input.workflow.permissions
  msg := "Missing top-level permissions"
}

warn[msg] {
  not input.workflow.concurrency
  msg := "Missing top-level concurrency"
}

deny[msg] {
  some i
  input.uses[i] == "actions/checkout@v1" or input.uses[i] == "actions/checkout@v2"
  msg := "Outdated actions/checkout (< v4)"
}

warn[msg] {
  some i
  re_match("^[^@]+@v\\d+$", input.uses[i])
  msg := "Action pinned by floating tag; prefer commit SHA"
}

deny[msg] {
  some j
  not input.jobs[j]."timeout-minutes"
  msg := sprintf("Job '%s' missing timeout-minutes", [j])
}

warn[msg] {
  some j
  not input.jobs[j].runs_on_adaptive
  msg := sprintf("Job '%s' does not use adaptive runs-on", [j])
}

# Block pull_request_target when repo is a fork
deny[msg] {
  input.repo_is_fork == true
  input.workflow.on.pull_request_target
  msg := "pull_request_target used with forks"
}
