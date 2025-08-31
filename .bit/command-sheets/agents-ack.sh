# .bit/command-sheets/agents-ack.sh
#!/usr/bin/env bash
set -euo pipefail
ack_dir=".bit/out/ack"
mkdir -p "$ack_dir"
date -u +"%FT%TZ" > "$ack_dir/requested.txt"

# Optional: notify local bot registry or external agent bus (non-blocking)
if [ -x tools/lol.git ]; then
  tools/lol.git banner || true
fi

echo "Acknowledgements requested from .bitbots and runners."
