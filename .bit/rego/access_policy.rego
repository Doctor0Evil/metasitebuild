package bit_hub.access
allow {
  input.tx.valid_blockchain_sig
  input.user.has_contract_access
}
default allow = false
