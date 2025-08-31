package bithub.banter

# Banter compliance means:
# 1. Pipeline declares needs_compliance: true
# 2. At least one step explicitly references a banter/celebration hook
#    (e.g., Laughter.exe, magic.lol, celebration scripts)
# 3. Banter hook is not commented out or disabled

default banter_ok = false

banter_ok {
  some path
  startswith(path, ".bit-actions/pipelines/")
  p := input.yaml[path].pipeline
  p.needs_compliance

  some step
  step := p.steps[_]

  # Match on step id, run command, or uses reference
  banter_step(step)
}

banter_step(step) {
  contains(lower(step.id), "banter")
} {
  step.run
  regex.match(`(?i)(laughter\.exe|magic\.lol|celebrat(e|ion))`, step.run)
} {
  step.uses
  regex.match(`(?i)(banter|celebration)`, step.uses)
}

deny[msg] {
  not banter_ok
  msg := "Pipeline missing banter-compliance hook (no Laughter.exe / celebration step found)"
}
