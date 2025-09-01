Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$PinsPath = "configs/pin/gha-pins.aln"
$CacheDir = "tools/actions/cache"
New-Item -ItemType Directory -Force -Path $CacheDir | Out-Null

$Pins = Get-Content -Raw -LiteralPath $PinsPath
# Expect simple ALN-ish lines: owner/repo@sha -> url
$Lines = $Pins -split "`n" | Where-Object { $_ -match "@" }

foreach ($line in $Lines) {
  $parts = $line.Trim() -split "\s+"
  $ref = $parts[0]       # e.g., actions/checkout@abc123
  $url = $parts[1]       # e.g., https://codeload.github.com/actions/checkout/zip/abc123 (mirrored)
  $ownerRepo = $ref.Split("@")[0]
  $sha = $ref.Split("@")[1]
  $dest = Join-Path $CacheDir ($ownerRepo.Replace("/", "_") + "_" + $sha + ".zip")

  if (-not (Test-Path $dest)) {
    Write-Host "Mirroring $ref"
    # Route through wall via local proxy (e.g., http://localhost:8088)
    $proxyUrl = "http://localhost:8088/fetch?url=" + [URI]::EscapeDataString($url)
    Invoke-WebRequest -Uri $proxyUrl -OutFile $dest
  } else {
    Write-Host "Cached $ref"
  }
}
