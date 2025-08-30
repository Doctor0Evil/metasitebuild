package policy.permissions

default wf_perms := {"contents": "read"}

deny[msg] {
  # top-level permissions must exist and include required
  required := data.security.policy.required_permissions
  some k
  required[k]
  not input.permissions[k]
  msg := sprintf("missing required permission at workflow level: %v", [k])
}

deny[msg] {
  # forbidden permissions anywhere (top-level or job-level)
  p := data.security.policy.forbidden_permissions[_]
  input.permissions[p]
  msg := sprintf("forbidden permission at workflow level: %v", [p])
}

deny[msg] {
  some job_name
  job := input.jobs[job_name]
  p := data.security.policy.forbidden_permissions[_]
  job.permissions[p]
  msg := sprintf("forbidden permission at job '%v': %v", [job_name, p])
}
