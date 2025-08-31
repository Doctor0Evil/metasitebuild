package bithub.content

deny[msg] {
    input.type == "text"
    contains_prohibited(input.text)
    msg := sprintf("Non-compliant term found: %v", [badword])
}
warn[msg] {
    input.type == "text"
    contains_profanebutcompliant(input.text)
    not input.metadata.tags["fictional"]
    msg := sprintf("Profane but compliant term without fictional tag: %v", [term])
}
