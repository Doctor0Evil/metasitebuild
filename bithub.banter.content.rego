package bithub.banter.content

default has_meta = false
has_meta { input.yaml["meta.bit.bang"] }

deny[msg] {
  not has_meta
  msg := "Missing meta.bit.bang manifest"
}

meta := input.yaml["meta.bit.bang"]

# Require creator attribution and corp governance
deny[msg] {
  not meta.meta.creator
  msg := "meta.bit.bang missing meta.creator"
}
deny[msg] {
  meta.meta.governed_by != "Bit.Hub.corp"
  msg := "meta.bit.bang must be governed_by Bit.Hub.corp"
}

# Content scanning report gate
findings := input.json[".bithub/reports/content-scan.json"].findings

deny[msg] {
  some i
  f := findings[i]
  f.level == "banned"
  msg := sprintf("Banned content detected: %s", [f.rule])
}

deny[msg] {
  some i
  f := findings[i]
  f.level == "restricted"
  not f.sanitized
  not f.review_approved
  msg := sprintf("Restricted content requires sanitization or review: %s", [f.rule])
}
