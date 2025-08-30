name: "Bit Cache Keys"
description: "Compute cache keys/paths from registry"
inputs:
  kind: { required: true }
outputs:
  key: { value: ${{ steps.mk.outputs.key }} }
  restore: { value: ${{ steps.mk.outputs.restore }} }
  paths: { value: ${{ steps.mk.outputs.paths }} }
runs:
  using: "composite"
  steps:
    - id: mk
      shell: bash
      run: |
        set -euo pipefail
        sudo apt-get update && sudo apt-get install -y yq jq >/dev/null
        REG=".bit-actions/bit.actions.registry.aln.yml"
        K="${{ inputs.kind }}"
        P=$(yq -r ".registry.caches[\"$K\"].paths[]" "$REG")
        F=$(yq -r ".registry.caches[\"$K\"].keys[]" "$REG")
        HASH="${{ hashFiles(format('{0}', inputs.kind)) }}"
        echo "paths=$(printf '%s\n' "$P")" >> "$GITHUB_OUTPUT"
        echo "key=bithub-${K}-${{ runner.os }}-${HASH}" >> "$GITHUB_OUTPUT"
        echo "restore=bithub-${K}-${{ runner.os }}-" >> "$GITHUB_OUTPUT"
