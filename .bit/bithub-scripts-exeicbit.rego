package bithub.scripts.execbit

critical_scripts := {
    "scripts/resync-ingestion.sh",
    ".bit/loaders/validate_loop.sh"
}

deny[msg] {
    script := critical_scripts[_]
    not file_exists(script)
    msg := sprintf("❌ Missing critical script: %s", [script])
}

deny[msg] {
    script := critical_scripts[_]
    file_exists(script)
    not input.files[_].path == script
    not input.files[_].mode & 0o111 > 0
    msg := sprintf("❌ Script not executable: %s", [script])
}

file_exists(path) {
    some f
    f := input.files[_]
    f.path == path
}
