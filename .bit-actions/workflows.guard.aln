aln:
  id: "aln://bithub/workflows.guard"
  version: "1.0.0"

rules:
  - id: "permissions-block"
    description: "Workflows must declare explicit permissions"
    match: ".github/workflows/**/*.yml"
    require:
      yaml_path: "permissions.contents"
      equals: "read"

  - id: "compliance-job"
    description: "Workflows must have a compliance job"
    match: ".github/workflows/**/*.yml"
    require:
      yaml_path: "jobs.compliance"

  - id: "needs-compliance"
    description: "All jobs must depend on compliance"
    match: ".github/workflows/**/*.yml"
    require:
      yaml_path: "jobs.*.needs"
      contains: "compliance"
