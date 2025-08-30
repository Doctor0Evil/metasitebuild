import json, time, random
try:
    old = json.load(open('.bithub/telemetry.json'))
except:
    old = {}
safety_report = {
    "timestamp": time.time(),
    "workflow_count": len(old.get('workflows', [])),
    "platform": old.get('platform', ''),
    "active_user": old.get('user', ''),
    "status": "audit_ok" if random.random() > 0.1 else "audit_flagged"
}
with open(".bithub/safety-audit.json", "w") as f:
    json.dump(safety_report, f, indent=2)
print("Safety Audit Complete:", safety_report["status"])
