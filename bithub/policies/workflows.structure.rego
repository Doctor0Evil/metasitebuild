package bithub.workflows.structure

deny[msg] {
  startswith(path, ".github/workflows/")
  wf := input.yaml[path]
  not wf.permissions
  msg := sprintf("Workflow missing explicit permissions: %s", [path])
}

deny[msg] {
  startswith(path, ".github/workflows/")
  wf := input.yaml[path]
  not has_compliance_job(wf)
  msg := sprintf("Workflow lacks a 'compliance' job: %s", [path])
}

deny[msg] {
  startswith(path, ".github/workflows/")
  wf := input.yaml[path]
  j := wf.jobs[_]
  name := key(j, wf.jobs)
  name != "compliance"
  not job_needs_compliance(j)
  msg := sprintf("Job '%s' does not declare needs: compliance", [name])
}

has_compliance_job(wf) {
  wf.jobs.compliance
}

job_needs_compliance(j) {
  j.needs == "compliance"
} {
  some i
  j.needs[i] == "compliance"
}
