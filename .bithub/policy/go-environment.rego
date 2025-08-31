package bithub.go_environment

default allow = false

allow {
  valid_go_version
  valid_modules
  compliance_enforced
  pipeline_valid
  runner_authorized
}

valid_go_version {
  version := input.go_version
  re_match("^1\\.(1[5-9]|[6-9][0-9])(\\.\\d+)?$", version)
}

valid_modules {
  all_modules := input.modules[_]
  all_modules.name != ""
  all_modules.version != ""
}

compliance_enforced {
  input.compliance.policy_version == "1.0.0"
  input.compliance.enforce_strict == true
  count(input.compliance.allowed_labels) > 0
}

pipeline_valid {
  count(input.pipeline.steps) > 0
  input.pipeline.timeout_minutes >= 10
  input.pipeline.timeout_minutes <= 120
  count(input.pipeline.cache.paths) > 0
  input.pipeline.cache.key != ""
}

runner_authorized {
  some label
  label := input.runner.labels[_]
  label == input.compliance.allowed_labels[_]
}

deny[msg] {
  not allow
  msg := "Go environment pipeline does not comply with Bit.Hub policies or runner is unauthorized."
}
