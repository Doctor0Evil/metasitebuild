package policy.triggers

deny[msg] {
  t := data.security.policy.required_triggers[_]
  not has_trigger(t)
  msg := sprintf("missing required trigger: %v", [t])
}

deny[msg] {
  t := data.security.policy.forbidden_triggers[_]
  has_trigger(t)
  msg := sprintf("forbidden trigger present: %v", [t])
}

has_trigger(t) {
  input.on[t]
}
