package bithub.tos

default compliant = false

# Load TOS rules from a manifest
tos := input.yaml[".bithub/legal/tos.yml"]

# Check all workflows for disallowed patterns
deny[msg] {
  startswith(path, ".github/workflows/")
  wf := input.yaml[path]
  some job
  job := wf.jobs[_]
  some step
  step := job.steps[_]
  step.run
  regex.match(tos.restricted_patterns[_], step.run)
  msg := sprintf("TOS violation in %s: pattern %q", [path, tos.restricted_patterns[_]])
}

# Allow if no denies triggered
compliant {
  not deny[_]
}
