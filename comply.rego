package bithub.compliance_wall

# Parameters to require for any operation
required_author := "Jacob Scott Corey Farmer"
approved_projects := {"ALNFantasia", "ALN's Framework"}
bitoperations_tag := "BitHub-Storage-Policy-V1"

# Core access rule: nothing passes if any compliance check fails
default allow = false

allow {
    # 1. Only the named author may push or trigger jobs
    input.author == required_author

    # 2. Project must be specifically allowed
    input.project_name in approved_projects

    # 3. All assets and workflows must be tagged
    contains(input.tags, bitoperations_tag)

    # 4. Every asset and code change must have explicit provenance
    input.asset_origin_verified == true

    # 5. ML workflow, runner, or asset must pass integrity check (e.g., SHA or signature)
    input.integrity_verified == true

    # 6. Real-time runner audit verification (all internal audit hooks return pass)
    all_audits_passed
}

# Utility for checking audit array
all_audits_passed {
    not failed_audits[_]
}
failed_audits[audit] {
    input.audit_logs[audit].status != "pass"
}

# Error messages for complete transparency
deny[msg] {
    not allow
    msg := sprintf(
      "Bit.Hub Compliance Wall DENIES: author=%v, project=%v, tags=%v, origin=%v, integrity=%v, audits=%v",
      [input.author, input.project_name, input.tags, input.asset_origin_verified, input.integrity_verified, input.audit_logs]
    )
}

# Helper for tag search
contains(arr, val) {
    some i
    arr[i] == val
}
