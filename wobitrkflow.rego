package workflows

# --- Deny rules ---

deny[msg] {
    not workflow_has_required_jobs
    msg := "❌ Workflow is missing one or more required jobs: validate, build-dotnet, security-scan"
}

deny[msg] {
    not workflow_has_correct_files_job
    msg := "❌ Workflow must include the 'correct-files' job"
}

deny[msg] {
    not workflow_has_security_scan
    msg := "❌ Workflow must include a security scan job"
}

deny[msg] {
    not workflow_has_branch_protection
    msg := "❌ Workflow must trigger only on main or develop branches"
}

deny[msg] {
    not security_scan_runs_before_deploy
    msg := "❌ 'security-scan' job must complete successfully before 'deploy'"
}

deny[msg] {
    not validate_job_has_readme_skip_logic
    msg := "❌ 'validate' job must include README.md skip logic for automation commits"
}

# --- Rules ---

workflow_has_required_jobs {
    required := {"validate", "build-dotnet", "security-scan"}
    jobs := {lower(name) | name := input.jobs[_].name}
    required ⊆ jobs
}

workflow_has_correct_files_job {
    some job
    job := input.jobs[_]
    lower(job.name) == "🛠️ aln file correction"
}

workflow_has_security_scan {
    some job
    job := input.jobs[_]
    lower(job.name) == "🔒 security scan"
}

workflow_has_branch_protection {
    branches := input.on.push.branches
    branches == ["main", "develop"]
}

security_scan_runs_before_deploy {
    deploy_job := input.jobs[_]
    lower(deploy_job.name) == "🚀 deploy"
    "security-scan" == lower(deploy_job.needs[_])
}

validate_job_has_readme_skip_logic {
    validate_job := input.jobs[_]
    lower(validate_job.name) == "🔍 validate & setup"
    some step
    step := validate_job.steps[_]
    contains(lower(step.run), "skip") 
    contains(lower(step.run), "readme.md") 
    contains(lower(step.run), "aln file corrector")
}
