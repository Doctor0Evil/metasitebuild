too_much_profanity {
  count(regex.find_all(`(?i)\bfuck\b`, input.content)) > 2
}

allow {
  has_required_signatures
  level_ok
  not_hate_speech
  verdict_pass
  not too_much_profanity
}
