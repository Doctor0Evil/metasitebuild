package bithub.humor

default threshold = "standard"

threshold = "paranoid" {
  contains_hot_word
}

threshold = "strict" {
  contains_medium_word
}

contains_hot_word {
  some w
  w := ["lol","omfg","wtf","lmao","lmfao","meltdown","chaos"]
  contains_word(input.commit_message, w)
}

contains_medium_word {
  some w
  w := ["bug","fail","error","panic"]
  contains_word(input.commit_message, w)
}

contains_word(text, word) {
  lower(text) contains lower(word)
}
