import os, json
import platform, getpass, socket
telemetry = {
    "user": getpass.getuser(),
    "host": socket.gethostname(),
    "platform": platform.platform(),
    "workflows": [],
}
# Example: scan workflow folders for usage
import glob
telemetry["workflows"] = glob.glob(".github/workflows/*.yml")
with open(".bithub/telemetry.json", "w") as f:
    json.dump(telemetry, f)
print("BitBot Telemetry Stored.")
