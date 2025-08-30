package bithub.santa

default allow_build = false

# Allow only repos on the nice list
allow_build {
  input.repo in nice_list
}

# Define the nice list
nice_list := {
  "aln://repo/ALNFantasia",
  "aln://repo/BitBot",
  "aln://repo/fan.asia.create"
}

# Naughty list detection
deny[msg] {
  not allow_build
  msg := sprintf("🎅 Naughty repo detected: %s", [input.repo])
}
