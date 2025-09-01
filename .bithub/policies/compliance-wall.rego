# .bithub/policies/compliance-wall.rego

package bithub.compliance

default allow = false

allowed_domains = [
  "packages.local",
  "nodes.virta",
  "time.cloudflare.com",
  "ghcr.io",
  "google.com",
  "*.googleapis.com",
  "*.microsoft.com",
  "login.microsoftonline.com",
  "graph.microsoft.com",
  "office.com",
  "outlook.com"
]

allow {
  some d
  input.domain == allowed_domains[d]
}
