package bithub.compliance.manifest

deny[msg] {
    not input.yaml[".bit/compliance.yml"]
    msg := "❌ Missing .bit/compliance.yml"
}

deny[msg] {
    cm := input.yaml[".bit/compliance.yml"]
    not cm.authority
    msg := "❌ compliance.yml missing 'authority'"
}

deny[msg] {
    cm := input.yaml[".bit/compliance.yml"]
    not cm.audit_trail
    msg := "❌ compliance.yml missing 'audit_trail'"
}
