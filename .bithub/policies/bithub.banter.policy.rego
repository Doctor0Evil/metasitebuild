package bithub.banter

default banter_ok = false

# Banter compliance means: pipeline declares needs_compliance
# AND has at least one banter/celebration hook step
banter_ok {
  some path
  startswith(path, ".bit-actions/pipelines/")
  p := input.yaml[path].pipeline
  p.needs_compliance
  some step
  step := p.steps[_]
  contains(lower(step.id), "banter")  # or match on run/uses patterns
}

deny[msg] {
  not banter_ok
  msg := "Pipeline missing banter-compliance hook"
}
