package rag_llm_deployment

default allow = false

# Domain allow/deny logic
deny_unapproved_domains {
    input.domain == denied_domains[_]
}

allow_approved_domains {
    input.domain == allowed_domains[_]
}

allow {
    allow_approved_domains
    not deny_unapproved_domains
}

# Strict RBAC enforcement
enforce_strict_rbac {
    input.role == allowed_roles[_]
}

# Encryption checks
check_encryption {
    input.encryption == "AES-256-GCM"
    re_match("^/opt/vsc/.*\\.asc$", input.key_path)
}

# Deny rule with message
deny[msg] {
    not allow
    msg := sprintf("Denied by compliance-wall on domain: %v", [input.domain])
}
