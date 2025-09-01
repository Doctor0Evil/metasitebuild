package bit.hardware_blocklist
deny[msg] {
  forbidden := {"MT6883", "arm64", "mediatek", "insecure_cyber"}
  some hw
  hw := input.inventory[_]
  forbidden[hw]
  msg := sprintf("Blocked hardware detected: %s", [hw])
}
package bit.compliance
deny[msg] {
  input.platform == "github"
  not input.complies_with_guidelines
  msg = "GitHub must comply—policy urgency enforced."
}
allow {
  not deny[_]
}
