@echo off
REM Windows batch wrapper to invoke loldeploy_wall.sh from the same directory
setlocal

REM Get directory of this script
set SCRIPT_DIR=%~dp0

REM Call the bash deployment script
bash "%SCRIPT_DIR%loldeploy_wall.sh"

endlocal
