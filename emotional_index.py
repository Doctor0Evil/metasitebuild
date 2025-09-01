import sys, json, math
from pathlib import Path

def clamp(x, lo=0.0, hi=1.0): return max(lo, min(hi, x))

def dist_to_box(v,a,d, box):
    vx = 0 if box["valence"][0] <= v <= box["valence"][1] else min(abs(v-box["valence"][0]), abs(v-box["valence"][1]))
    ax = 0 if box["arousal"][0] <= a <= box["arousal"][1] else min(abs(a-box["arousal"][0]), abs(a-box["arousal"][1]))
    dx = 0 if box["dominance"][0] <= d <= box["dominance"][1] else min(abs(d-box["dominance"][0]), abs(d-box["dominance"][1]))
    # normalize worst-case distance of 1.0 per axis
    return 1 - clamp((vx+ax+dx)/3)

def main():
    emo = json.loads(Path("data/emotion_states.json").read_text())
    pers = json.loads(Path("data/personality_vectors.json").read_text())
    persona = pers["personae"][0]  # simple: one persona target for now

    leaks, outliers, drift = [], [], []
    allowed = set(persona["allowed_labels"])
    forbidden = set(persona["forbidden_labels"])
    zones = {z["label"]: z["max_intensity"] for z in persona.get("leakage_zones", [])}

    align_scores = []
    allowed_ratio_acc = []
    last_vad = None

    for ev in emo["events"]:
        v,a,d = ev["vad"]["valence"], ev["vad"]["arousal"], ev["vad"]["dominance"]
        labels = ev.get("labels", [])
        conf = ev.get("confidence", 1.0)

        # forbidden leakage
        forb_intersect = [l for l in labels if l in forbidden and conf >= 0.7]
        for l in forb_intersect:
            leaks.append({"ref": f"{ev.get('text_ref','?')}:{ev.get('line', '?')}", "label": l, "confidence": conf})

        # leakage zone caps
        for l in labels:
            cap = zones.get(l)
            if cap is not None and conf > cap:
                leaks.append({"ref": f"{ev.get('text_ref','?')}:{ev.get('line','?')}", "label": l, "confidence": conf, "cap": cap})

        # persona VAD alignment
        align_scores.append(dist_to_box(v,a,d, persona["vad_target"]))

        # allowed label ratio
        if labels:
            allowed_ratio_acc.append(sum(1 for l in labels if l in allowed)/len(labels))

        # drift detection
        if last_vad:
            delta = sum(abs(x-y) for x,y in zip((v,a,d), last_vad))/3
            if delta > persona["drift"]["max_spike"]:
                drift.append({"ref": ev.get("text_ref","?"), "delta": delta})
        last_vad = (v,a,d)

    EIS = 1.0 if not leaks else max(0.0, 1.0 - min(1.0, 0.2*len(leaks)))
    PAS = 0.5*(sum(align_scores)/max(1,len(align_scores))) + 0.5*(sum(allowed_ratio_acc)/max(1,len(allowed_ratio_acc)))
    CCS = 1.0 if not drift else max(0.0, 1.0 - min(1.0, 0.5*len(drift)))

    out = {
      "version": 1,
      "scores": {"emotional_integrity": round(EIS,3),
                 "persona_alignment": round(PAS,3),
                 "context_consistency": round(CCS,3)},
      "signals": {"forbidden_affect_leakage": leaks, "outliers": outliers, "drift_events": drift}
    }
    Path("logs").mkdir(exist_ok=True)
    Path("logs/emotion_personality_audit.json").write_text(json.dumps(out, indent=2))
    print("✅ emotional_index complete")
    sys.exit(0 if not leaks else 0)  # let policy decide in contextual audit

if __name__ == "__main__":
    main()
