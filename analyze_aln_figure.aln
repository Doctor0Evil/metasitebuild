import sys, json, yaml, hashlib, re, os
from pathlib import Path

def sha256_file(p): 
    h = hashlib.sha256()
    with open(p, "rb") as f:
        for chunk in iter(lambda: f.read(1<<20), b""):
            h.update(chunk)
    return h.hexdigest()

def read_yaml(p): 
    with open(p) as f: return yaml.safe_load(f)

def read_json(p): 
    with open(p) as f: return json.load(f)

def assert_true(cond, msg, errs):
    if not cond: errs.append(msg)

def main():
    fig = read_yaml("config/.aln-compliance-figure")["compliance_figure"]
    errs = []
    # Charter invariant
    for inv in fig.get("invariants", []):
        if inv.get("must_match"):
            path = inv.get("applies_to")
            assert_true(Path(path).exists(), f"Missing {path}", errs)
            txt = Path(path).read_text(encoding="utf-8", errors="ignore")
            assert_true(re.search(inv["must_match"], txt) is not None,
                        f"Invariant failed: {inv['key']}", errs)
        elif inv.get("json_path"):
            jf = inv["file"]; assert_true(Path(jf).exists(), f"Missing {jf}", errs)
            data = read_json(jf)
            # simple paths only: $.key
            key = inv["json_path"].lstrip("$.")
            val = data.get(key.split(".")[0])
            assert_true(val == inv["equals"], f"Invariant failed: {inv['key']}", errs)
    # Persona lock
    lock = fig["bindings"]["persona_registry"]["lock_file"]
    reg = fig["bindings"]["persona_registry"]["file"]
    lock_key = fig["bindings"]["persona_registry"]["lock_key"]
    assert_true(Path(lock).exists() and Path(reg).exists(), "Missing persona lock/registry", errs)
    lock_txt = Path(lock).read_text()
    want = None
    for line in lock_txt.splitlines():
        if line.startswith(lock_key+"="):
            want = line.split("=",1)[1].strip()
    got = sha256_file(reg)
    assert_true(want is not None, "registry_sha256 missing", errs)
    assert_true(want == got, "Persona registry hash mismatch", errs)
    # Runtime rule
    forbid_dir = fig["runtime_rules"]["forbid_dir_populated"]
    if Path(forbid_dir).exists() and any(Path(forbid_dir).iterdir()):
        errs.append(f"Runtime artifacts present in CI: {forbid_dir}")

    for e in errs: print(f"❌ {e}")
    if errs:
        print("Figure analysis: FAIL")
        sys.exit(1)
    print("✅ Figure analysis: PASS")

if __name__ == "__main__":
    main()
