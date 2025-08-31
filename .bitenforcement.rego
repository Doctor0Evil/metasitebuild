package bithub.compliancewall

default allow = false
allow {
    input.author == "REQUIRED_AUTHOR"
    input.projectname == "ALLOWED_PROJECT"
    contains(input.tags, "bitoperationstag")
    input.assetoriginverified == true
    input.integrityverified == true
    all_audits_pass
}
all_audits_pass {
    not failed_audits
}
failed_audits[audit] {
    input.auditlogs[audit].status != "pass"
}
deny[msg] {
    not allow
    msg := sprintf("Bit.Hub Compliance Wall DENIES: author=%v, project=%v, tags=%v", [input.author, input.projectname, input.tags])
}
