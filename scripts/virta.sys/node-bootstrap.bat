@echo off
setlocal enabledelayedexpansion
REM Harden host, start wall, start gossip, verify pins

REM 1) Time sync (optional) and firewall defaults
w32tm /resync >NUL 2>&1
netsh advfirewall set allprofiles state on
netsh advfirewall firewall add rule name="Deny Outbound by Default" dir=out action=block profile=any

REM 2) Allow local mirrors and federation
for %%D in ("packages.local" "nodes.virta" "time.cloudflare.com") do (
  netsh advfirewall firewall add rule name="Allow %%D 443" dir=out action=allow remoteip=%%D protocol=TCP localport=any remoteport=443
)

REM 3) Start egress guard proxy (assume service installed)
sc start EgressGuard

REM 4) Start gossip daemon
start "" /B tools\gossip\gossipd.exe -listen :7777 -peers nodes.virta:7777 -topics ledger,asset-index

REM 5) Verify wall policy signature
tools\ed25519\ed25519-cli.exe verify -pub .keys\ed25519.public -msg (certutil -hashfile manifests\compliance.wall.aln SHA256 | findstr /R "^[0-9A-F]") -sig (type .keys\compliance.sign 2>nul)

REM 6) Mirror actions and toolchains
powershell -ExecutionPolicy Bypass -File scripts\actions-mirror.ps1

REM 7) Run watchdog
start "" /B runtimes\lisp.exe services\wall\watchdog.lisp

echo virta.sys node bootstrap complete.
endlocal
