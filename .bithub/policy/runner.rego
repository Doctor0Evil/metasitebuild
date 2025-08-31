package bithub.runner

# Policy input shape:
# { kind: "runner", system: {...}, github_runner: {...}, tools: {...} }

# Ensure required labels are present (Bit.Hub fleet hint)
warn[msg] {
  not runner_has_label("bit.hub")
  msg := "Runner missing 'bit.hub' label"
}

# Recommend modern OS/kernel
warn[msg] {
  not input.system.kernel
  msg := "Kernel not detected; baseline unknown"
}

# Discourage docker privileged modes on CI (best-effort)
warn[msg] {
  input.tools.docker.present == true
  input.tools.docker.privileged == true
  msg := "Docker privileged detected; disallow for CI"
}

runner_has_label(lbl) {
  contains(lower(input.github_runner.labels), lbl)
}
