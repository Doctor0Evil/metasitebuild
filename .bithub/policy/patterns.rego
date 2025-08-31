package bithub.patterns

# Input:
# {
#   "path": ".github/workflows/x.yml",
#   "workflow": {...},             # parsed YAML as JSON
#   "patterns": {...}              # loaded from .bit/patterns/workflow-environments.yml
# }

deny[msg] {
  some i
  re_match("actions/checkout@v(1|2)$", input.uses[i])
  msg := "Outdated actions/checkout (< v4)"
}

warn[msg] {
  some i
  re_match("^[^@]+@v\\d+$", input.uses[i])
  msg := "Floating action tag; prefer commit SHA"
}

warn[msg] {
  not input.workflow.concurrency
  msg := "Missing top-level concurrency; consider adding group/cancel-in-progress"
}

deny[msg] {
  some j
  not input.workflow.jobs[j]."timeout-minutes"
  msg := sprintf("Job '%s' missing timeout-minutes", [j])
}

# Environment recognition (advisory)
env_detected(e) {
  e := environment_match(input.workflow, input.patterns.environments)
}

warn[msg] {
  not env_detected(_)
  msg := "No environment detected; consider annotating name or steps"
}

# Helper (pseudo) – actual matching done by the recognizer composite;
# policy keeps only the logical checks, not YAML edits.
