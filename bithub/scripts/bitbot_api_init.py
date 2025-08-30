import os, json
with open('.bithub/bitbot_status.json', 'w') as f:
    json.dump({"status": "initialized", "timestamp": __import__('datetime').datetime.utcnow().isoformat()}, f)
print("BitBot API Initialized.")
