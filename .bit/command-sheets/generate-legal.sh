# .bit/command-sheets/generate-legal.sh
#!/usr/bin/env bash
set -euo pipefail
KIND="${1:-ALN}"
assets_dir=".bit/assets/${KIND}"
schema=".bit/schemas/asset.legal.json"
tpl=".bit/templates/legal/TEMPLATE.md"
out_dir="docs/legal/${KIND}"
art_dir=".bit/out/legal/${KIND}"
mkdir -p "$out_dir" "$art_dir"

shopt -s nullglob
count=0
for meta in "$assets_dir"/*.yml "$assets_dir"/*.yaml "$assets_dir"/*.json; do
  [ -f "$meta" ] || continue
  id="$(basename "$meta" | sed 's/\.[^.]*$//')"
  out_md="$out_dir/${id}.md"
  out_json="$art_dir/${id}.json"

  # Best-effort normalize JSON for template rendering
  case "$meta" in
    *.json) cp "$meta" "$out_json" ;;
    *) yq -o=json '.' "$meta" > "$out_json" ;;
  esac || true

  # Optional: schema validation (non-blocking)
  if command -v ajv >/dev/null 2>&1; then
    ajv validate -s "$schema" -d "$out_json" || echo "::warning ::Schema validation warnings for $id"
  fi

  # Render markdown (lightweight mustache via jq)
  jq -r --argfile d "$out_json" '
    def get($p): getpath($p) // "";
  ' /dev/null >/dev/null 2>&1 || true

  # Simple templating with envsubst-style placeholders via jq
  banter=$(jq -r '.personality.banter // ""' "$out_json")
  profanity=$(jq -r '.personality.profanity // ""' "$out_json")
  rep=$(jq -r '.personality.rep // ""' "$out_json")
  compscore=$(jq -r '.personality.compscore // ""' "$out_json")
  quirk=$(jq -r '.personality.quirk // ""' "$out_json")
  owner=$(jq -r '.owner // ""' "$out_json")
  repo=$(jq -r '.repository // ""' "$out_json")
  license=$(jq -r '.license // ""' "$out_json")
  maturity=$(jq -r '.compliance.maturity // ""' "$out_json")
  refs=$(jq -r '.compliance.references // [] | join(", ")' "$out_json")

  sed -e "s/{{id}}/${id}/g" \
      -e "s/{{kind}}/${KIND}/g" \
      -e "s/{{owner}}/${owner}/g" \
      -e "s/{{repository}}/${repo}/g" \
      -e "s/{{license}}/${license}/g" \
      -e "s/{{personality.banter}}/${banter}/g" \
      -e "s/{{personality.profanity}}/${profanity}/g" \
      -e "s/{{personality.rep}}/${rep}/g" \
      -e "s/{{personality.compscore}}/${compscore}/g" \
      -e "s/{{personality.quirk}}/${quirk}/g" \
      -e "s/{{compliance.maturity}}/${maturity}/g" \
      -e "s/{{compliance.toggles}}/[see metadata]/g" \
      -e "s/{{compliance.references}}/${refs}/g" \
      "$tpl" > "$out_md"

  echo "Generated: $out_md"
  count=$((count+1))
done

echo "Generated $count legal docs for kind ${KIND}."
