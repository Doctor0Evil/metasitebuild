package bithub.personalitycore
default allow = false
allow {
    input.commit.author == "Jacob Scott Farmer"
    input.asset.type == "personalityvector"
    input.asset.origin == "Bit.Hub"
    not input.external_claim
}
deny[msg] {
    not allow
    msg := sprintf("Access denied: Only Bit.Hub authorized agents can mutate 'personality-vectors'. External interference or claims are forbidden.")
}
