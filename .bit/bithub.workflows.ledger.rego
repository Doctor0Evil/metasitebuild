package bithub.workflows.ledger

deny[msg] {
    startswith(path, ".github/workflows/")
    wf := input.yaml[path]
    not some step in wf.jobs[_].steps
    not ledger_step(step)
    msg := sprintf("❌ Workflow %s missing ledger logging step", [path])
}

ledger_step(step) {
    contains(step.run, ".bithub/ledger/")
}
