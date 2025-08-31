package bithub.runner

warn[msg] {
  not runner_has_label("bit.hub")
  msg := "Runner missing required 'bit.hub' label"
}

warn[msg] {
  not input.system.kernel
  msg := "Runner kernel information missing"
}

warn[msg] {
  input.tools.docker.present == true
  input.tools.docker.privileged == true
  msg := "Privileged Docker mode detected on runner; discouraged for CI"
}

runner_has_label(lbl) {
  contains(lower(input.github_runner.labels), lbl)
}
