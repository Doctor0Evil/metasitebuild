#!/usr/bin/env bash
set -euo pipefail

MANIFEST=".bit/runner.manifest.yml"
POLICIES=$(yq '.runner.compliance.policies_dir' "$MANIFEST")

echo "[Bit.Hub.runner] Booting from $MANIFEST"
echo "[Bit.Hub.runner] Loading compliance policies from $POLICIES"
opa run --server --addr :8181 $POLICIES &

echo "[Bit.Hub.runner] Registering with linking.repo.bit mesh..."
# Simulated registration
sleep 1
echo "[Bit.Hub.runner] Ready to accept jobs."

while true; do
  JOB=$(bithub-job-fetch) # hypothetical job fetcher
  if [[ -n "$JOB" ]]; then
    echo "[Bit.Hub.runner] Executing job: $JOB"
    # Run job...
    if [[ $? -eq 0 ]]; then
      echo "[Bit.Hub.runner] Job succeeded."
      if [[ "$(yq '.runner.entertainment.trigger_on_success' "$MANIFEST")" == "true" ]]; then
        magic-lol
      fi
    else
      echo "[Bit.Hub.runner] Job failed."
    fi
  fi
  sleep 5
done
