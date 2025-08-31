package alnfantasia.dialogue

# Allow only if mature content flag enabled
allow_banter {
  input.user_flags.max_banter == true
}

deny_banter[reason] {
  not allow_banter
  reason := "Profane banter disabled—does not meet platform maturity requirements."
}

# Always exclude lines on reject list or hate speech patterns
deny_banter[reason] {
  banned := {"hate-speech", "explicit-identity-threat", "illegal-content"}
  input.banter_class in banned
  reason := "Banned banter class: " + input.banter_class
}
