# .bit/loaders/decompress_wrapper.sh
set -euo pipefail
ACTION="${1:-decompress}"
METHOD="${2:-zlib}"
INPUT="${3:-input.bin}"
OUTPUT="${4:-output.dat}"

opa eval --data .bithub/policies/decompression.rego "data.bithub.decompression.allow" \
  --input <(echo '{"action":"'$ACTION'","method":"'$METHOD'","repo":"'"$GITHUB_REPOSITORY"'","size_mb":'$(du -m "$INPUT" | cut -f1)'}') --format raw | grep true \
  || { echo "Blocked by Bit.Hub compliance policy"; exit 1; }

case "$METHOD" in
  zlib)    fan.asia.decompress::bitdecomp-zlib "$INPUT" "$OUTPUT" ;;
  gzip)    fan.asia.decompress::bitdecomp-gzip "$INPUT" "$OUTPUT" ;;
  tar)     fan.asia.decompress::bitdecomp-tar "$INPUT" "$OUTPUT" ;;
  nsartifact) fan.asia.decompress::bitdecomp-ns-artifact "$INPUT" "$OUTPUT" ;;
  *) echo "Unknown decompression method"; exit 2 ;;
esac
