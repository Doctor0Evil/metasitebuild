package policy.concurrency

deny[msg] {
  data.security.policy.concurrency.require_top_level
  not input.concurrency
  msg := "top-level concurrency required"
}

deny[msg] {
  input.concurrency
  data.security.policy.concurrency.require_cancel_in_progress
  cip := input.concurrency["cancel-in-progress"]
  not cip
  msg := "concurrency.cancel-in-progress must be true"
}
Save as: .bithub-actions/policy/concurrency.rego

Step hardening (curl|bash, secrets echo)
rego
package policy.steps

import future.keywords.in

# Disallow curl|bash or wget|sh from non-allowlisted domains
deny[msg] {
  some job_name
  job := input.jobs[job_name]
  s := job.steps[_]
  s.run
  re_match("(?i)(curl|wget).+\\|\\s*(bash|sh)", s.run)
  not allowlisted_domain(s.run)
  msg := sprintf("dangerous installer pattern in job '%v' step '%v'", [job_name, step_name(s)])
}

allowlisted_domain(run) {
  domain := regex.find_string_submatch("https?://([^/]+)/", run)[1]
  domain in data.security.policy.allowlist_domains
}

# Warn (not deny) if run appears to echo secrets
warn[msg] {
  some job_name
  job := input.jobs[job_name]
  s := job.steps[_]
  s.run
  re_match("(?i)echo\\s+.*secrets\\.", s.run)
  msg := sprintf("potential secret echoed in job '%v' step '%v'", [job_name, step_name(s)])
}

step_name(s) := n {
  n := s.name
} else := "unnamed"
