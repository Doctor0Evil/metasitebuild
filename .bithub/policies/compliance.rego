package bithub

default allow := false

# Input: .bithub/audit/humor-reasoning-trace.json
# {
#   "schema":"bithub.trace.v1",
#   "component":"humor.reasoning.compliance",
#   "artefacts": { ... optional ... },
#   "signatures":[{"key_id":"owner:bithub","alg":"RS256","signature":"..."}, {"key_id":"owner:perplexity","alg":"RS256","signature":"..."}],
#   "complianceLevel":"strict", ...
# }

# Input 2: OPA result file merged into eval context (or fetched on disk)
# {
#   "decisionPath":"data.bithub.allow",
#   "pass": true,
#   ...
# }

required_owners := {"owner:bithub", "owner:perplexity"}

has_required_signatures {
  some s
  input.signatures[s].key_id == "owner:bithub"
  some t
  input.signatures[t].key_id == "owner:perplexity"
}

level_ok {
  lvl := lower(input.complianceLevel)
  lvl == "standard"  # lowest acceptable
} or {
  lvl := lower(input.complianceLevel)
  lvl == "strict"
} or {
  lvl := lower(input.complianceLevel)
  lvl == "paranoid"
}

not_hate_speech {
  not regex.match(`(?i)\b(hate|genocide|slur_[a-z]+)\b`, input.content)
}

verdict_pass {
  input.opa.pass == true
}

allow {
  has_required_signatures
  level_ok
  not_hate_speech
  verdict_pass
}
