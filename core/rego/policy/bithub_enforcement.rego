package bithub.security

allow_command[msg] {
  input.command in command_allow_list
  msg := "Allowed"
}

command_allow_list := {
  ".bit push", ".bit merge", ".bit bot deploy", ".bit policy audit", ".bit vault sync",
  ".bit banter.allow", ".bit dice.roll", ".bit act.out", ".bit enforce", ".bit shield", ".bit trial"
}

bar_raised[msg] {
  input.requested_bar > input.limit_bar
  msg := "No further bar-raising allowed. Policy enforcement triggered."
}
