package bithub

# Default decision is to deny
default allow := false

# Owners that must sign the trace
required_owners := {"owner:bithub", "owner:perplexity"}

# Both required owners have signed
has_required_signatures {
    some s
    input.signatures[s].key_id == "owner:bithub"
    some t
    input.signatures[t].key_id == "owner:perplexity"
}

# Compliance level is one of the accepted values
level_ok {
    lvl := lower(input.complianceLevel)
    lvl == "standard"
} or {
    lvl := lower(input.complianceLevel)
    lvl == "strict"
} or {
    lvl := lower(input.complianceLevel)
    lvl == "paranoid"
}

# Content does not contain prohibited hate/abuse patterns
not_hate_speech {
    not regex.match(`(?i)\b(hate|genocide|slur_[a-z]+)\b`, input.content)
}

# The embedded OPA result says pass=true
verdict_pass {
    input.opa.pass == true
}

# Final allow rule: all conditions must be true
allow {
    has_required_signatures
    level_ok
    not_hate_speech
    verdict_pass
}
