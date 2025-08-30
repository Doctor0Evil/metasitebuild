package policy.runners

deny[msg] {
  some job_name
  job := input.jobs[job_name]
  r := job["runs-on"]
  not r_allowed(r)
  msg := sprintf("runner not allowed for job '%v': %v", [job_name, r])
}

r_allowed(r) {
  r in data.security.policy.allowed_runners
}
