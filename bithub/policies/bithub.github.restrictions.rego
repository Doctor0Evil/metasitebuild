package bithub.github.restrictions

default safelisted = false

deny[msg] {
  startswith(path, ".github/workflows/")
  step := input.yaml[path].jobs[_].steps[_]
  step.uses
  not allowed(step.uses)
  msg := sprintf("Disallowed action %q in %s", [step.uses, path])
}

allowed(a) {
  a == "actions/checkout@v4"
} {
  startswith(a, "actions/setup-")
} {
  startswith(a, "actions/cache@v4")
} {
  startswith(a, "actions/upload-artifact@v4")
} {
  startswith(a, "./.github/actions/")
}

deny[msg] {
  startswith(path, ".github/workflows/")
  step := input.yaml[path].jobs[_].steps[_]
  step.run
  regex.match(`(?i)(curl|wget).*\|.*bash`, step.run)
  msg := sprintf("Network pipe to shell prohibited in %s", [path])
}

deny[msg] {
  some f
  f := input.files[_].path
  contains(f, ".github\\workflows")
  msg := sprintf("Backslash in workflows path: %s", [f])
}
