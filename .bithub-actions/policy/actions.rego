package policy.actions

import future.keywords.in

deny[msg] {
  some job_name
  job := input.jobs[job_name]
  s := job.steps[_]
  s.uses
  not allowed_action(s.uses)
  msg := sprintf("action not allowlisted: %v", [s.uses])
}

deny[msg] {
  some job_name
  job := input.jobs[job_name]
  s := job.steps[_]
  s.uses
  not pinned(s.uses)
  msg := sprintf("action not pinned to a version: %v", [s.uses])
}

allowed_action(a) {
  a in data.security.policy.allowlist_actions
}

pinned(a) {
  re_match(".+@v[0-9][0-9.]*$", a)
}
