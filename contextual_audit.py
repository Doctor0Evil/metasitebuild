import sys, json, yaml, hashlib
from pathlib import Path
from datetime import datetime, timezone

def read_yaml(p): 
    with open(p) as f: return yaml.safe_load(f)
def read_json(p): 
    with open(p) as f: return json.load(f)

def main():
    fig = read_yaml("config/.aln-compliance-figure")["compliance_figure"]
    pol = read_yaml("config/aln_compliance_policy.aln")
    emo = read_json("logs/emotion_personality_audit.json")

    w = fig["scoring"]["weights"]
    s = emo["scores"]
    overall = w["emotional_integrity"]*s["emotional_integrity"] + \
              w["persona_alignment"]*s["persona_alignment"] + \
              w["context_consistency"]*s["context_consistency"]

    thr = fig["enforcement"]["thresholds"]
    leaks = emo["signals"]["forbidden_affect_leakage"]
    drift = emo["signals"]["drift_events"]

    violations = []
    if leaks: violations.append({"policy":"E-01","count":len(leaks)})
    if any(e.get("cap") for e in leaks): violations.append({"policy":"E-02"})
    if any(drift): violations.append({"policy":"P-03"})
    if overall < thr["overall_score_min"]: violations.append({"policy":"C-03"})

    verdict = "pass" if not violations and overall >= thr["overall_score_min"] else "fail"

    # Redacted examples
    masked_examples = []
    for l in leaks[:5]:
        masked_examples.append({"ref": l.get("ref","?"), "line": "<masked_line>", "reason": f"label {l.get('label')} confidence {l.get('confidence')}"})

    audit = {
      "version": 1,
      "summary": {
        "emotional_integrity": s["emotional_integrity"],
        "persona_alignment": s["persona_alignment"],
        "context_consistency": s["context_consistency"],
        "overall_score": round(overall,3),
        "verdict": verdict
      },
      "violations": violations,
      "signals": {
        "forbidden_affect_leakage": len(leaks),
        "drift_events": len(drift),
        "masked_examples": masked_examples
      },
      "meta": {
        "figure_id": fig["id"] if "id" in fig else "ALN-FIG",
        "policy_count": len(pol.get("policies",[])),
        "ts": datetime.now(timezone.utc).isoformat()
      }
    }

    Path("logs").mkdir(exist_ok=True)
    Path(fig["emit_audit"]["json"]).write_text(json.dumps(audit, indent=2))
    with open(fig["emit_audit"]["report"], "w") as f:
        f.write(f"ALN Context Compliance Report\n")
        f.write(f"Overall: {audit['summary']['overall_score']} Verdict: {verdict}\n")
        for v in violations:
            f.write(f"- Violation {v['policy']}\n")
        for ex in masked_examples:
            f.write(f"- {ex['ref']}: {ex['line']} ({ex['reason']})\n")

    mode = fig["enforcement"]["mode"]
    if mode == "audit": 
        sys.exit(0)
    elif mode == "repair_then_audit":
        # Placeholder: additional repair/quarantine could occur here
        sys.exit(0)
    else: # block
        sys.exit(1 if verdict == "fail" else 0)

if __name__ == "__main__":
    main()
