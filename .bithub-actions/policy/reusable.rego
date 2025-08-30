Matrix limits and parallelism
rego
package policy.matrix

deny[msg] {
  some job_name
  job := input.jobs[job_name]
  job.strategy.matrix.include
  cnt := count(job.strategy.matrix.include)
  cnt > data.security.policy.matrix.max_include
  msg := sprintf("job '%v' matrix.include too large: %v > %v", [job_name, cnt, data.security.policy.matrix.max_include])
}

deny[msg] {
  some job_name
  job := input.jobs[job_name]
  job.strategy["max-parallel"]
  mp := to_number(job.strategy["max-parallel"])
  mp > data.security.policy.matrix.max_parallel
  msg := sprintf("job '%v' strategy.max-parallel too high: %v > %v", [job_name, mp, data.security.policy.matrix.max_parallel])
}
Save as: .bithub-actions/policy/matrix.rego

Reusable workflow enforcement (.bithub hooks)
rego
package policy.reusable

# Require that orchestration workflows invoke .bithub compliance and preflight
deny[msg] {
  is_orchestration
  not calls_template("./.bithub-actions/templates/compliance-gate.yml")
  msg := "orchestration must call .bithub compliance-gate"
}

deny[msg] {
  is_orchestration
  not calls_template("./.bithub-actions/templates/preflight.yml")
  msg := "orchestration must call .bithub preflight"
}

is_orchestration {
  contains(lower(input.name), "orchestration")
}

calls_template(tpl) {
  some job_name
  job := input.jobs[job_name]
  job.uses == tpl
}
