package bithub.legal.loop

meta := input.yaml["meta.bit.bang"]
docs := meta.legalDocs.documents

deny[msg] {
  not meta.legalDocs.continuousLoop
  msg := "Legal docs must run in continuousLoop"
}

# Ensure declared legal documents exist in the repo (YAML/MD tracked via presence report)
presence := input.json[".bithub/reports/legal-docs.json"]

deny[msg] {
  some d
  d := docs[_]
  not presence.exists[d.path]
  msg := sprintf("Required legal doc missing: %s", [d.path])
}

deny[msg] {
  meta.legalDocs.attestations.require
  some a
  a := meta.legalDocs.attestations.files[_]
  not presence.exists[a]
  msg := sprintf("Required legal attestation missing: %s", [a])
}
