package bithub.bit_actions

default ok = false

ok {
  input.yaml[".bit-actions/bit.actions.registry.aln.yml"]
  input.yaml[".bit-actions/pipelines/site.build.aln.yml"]
}

deny[msg] {
  not ok
  msg := "Missing .bit-actions registry or pipeline manifests"
}

deny[msg] {
  p := input.yaml[".bit-actions/pipelines/site.build.aln.yml"].pipeline
  not p.needs_compliance
  msg := "Pipeline must require compliance gate"
}

deny[msg] {
  reg := input.yaml[".bit-actions/bit.actions.registry.aln.yml"].registry
  safelist := {s | s := reg.safelist_actions[_]}
  some path
  startswith(path, ".github/workflows/")
  step := input.yaml[path].jobs[_].steps[_]
  step.uses
  not safelist[step.uses]
  msg := sprintf("Action not safelisted by Bit.Hub registry: %s", [step.uses])
}
