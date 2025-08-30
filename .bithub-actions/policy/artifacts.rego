package policy.artifacts

artifact_steps[s] {
  some job_name
  job := input.jobs[job_name]
  s := job.steps[_]
  s.uses
  startswith(s.uses, "actions/upload-artifact@")
}

deny[msg] {
  s := artifact_steps[_]
  data.security.policy.artifacts.require_name
  not s.with.name
  msg := "upload-artifact requires 'with.name'"
}

deny[msg] {
  s := artifact_steps[_]
  data.security.policy.artifacts.require_path
  not s.with.path
  msg := "upload-artifact requires 'with.path'"
}

deny[msg] {
  s := artifact_steps[_]
  rd := to_number(s.with["retention-days"])
  min := data.security.policy.artifacts.min_retention_days
  max := data.security.policy.artifacts.max_retention_days
  not rd_valid(rd, min, max)
  msg := sprintf("upload-artifact retention-days must be between %v and %v", [min, max])
}

rd_valid(rd, min, max) {
  rd >= min
  rd <= max
}
