package bithub.mesh

default allow_job = false

allow_job {
  input.peer.persona == "strict"
  input.msg.payload.required.compliance == "critical"
  not forbidden_repo(input.msg.payload.repo)
}

forbidden_repo(repo) {
  endswith(repo, "untrusted")
}
