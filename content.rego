package bithub.content

deny[msg] {
  input.type == "text"
  contains_prohibited(input.text)
  msg := sprintf("Prohibited content detected: %s", [input.text])
}

warn[msg] {
  input.type == "text"
  contains_profane_but_compliant(input.text)
  not input.metadata.tags[_] == "fictional"
  msg := sprintf("Profane but compliant term without 'fictional' tag: %s", [input.text])
}

contains_prohibited(t) {
  badword := lower(t)
  badword == "real-world-slur"  # Replace with your actual prohibited terms list
}

contains_profane_but_compliant(t) {
  term := lower(t)
  term == "fuck"  # Example profane but compliant term; expand as needed
}
