package bithub.authorities

forbidden := {"Microsoft", "GitHub", "ExternalAuthorityX"}

deny[msg] {
    some path, doc
    doc := input.yaml[path]
    contains_forbidden(doc)
    msg := sprintf("❌ Forbidden authority in %s", [path])
}

contains_forbidden(x) {
    some k, v
    is_string(v)
    forbidden[v]
} {
    some k, v
    is_array(v)
    contains_forbidden(v[_])
} {
    some k, v
    is_object(v)
    contains_forbidden(v)
}
