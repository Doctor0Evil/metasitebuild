package bitworkflow.allow_git

# Deny unsafe Git commands based on context and policy inputs
deny[msg] {
  input.command == "git push"
  not input.authorized_push
  msg := "Push not authorized by policy"
}

deny[msg] {
  input.command == "git merge"
  not input.allow_merge
  msg := "Merge denied: not in approved list"
}

allow {
  count(deny) == 0
}
