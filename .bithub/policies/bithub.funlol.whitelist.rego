package bithub.funlol

# Default: not exempt
default exempt = false

# Mark this module's ledger entries as exempt from profanity checks
exempt {
  input.module.id == "aln://modules/fun.lol"
  input.module.compliance.profanity_allowed == true
}

# Allow specific profane terms if they appear in allowed_profanity list
allow_profanity[term] {
  exempt
  term := lower(input.term)
  term == allowed[_]
}

allowed := { t |
  t := lower(x)
  x := input.module.content.allowed_profanity[_]
}

# Deny if profanity is found and not in allowlist
deny[msg] {
  profanity := lower(input.term)
  not allow_profanity[profanity]
  msg := sprintf("Profanity '%s' not allowed for module %s", [profanity, input.module.id])
}
