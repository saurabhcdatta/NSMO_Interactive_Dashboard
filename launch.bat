@echo off
setlocal EnableDelayedExpansion

:: ============================================================
::  NSMO Dashboard Launcher v8 — Final
:: ============================================================

set APP_NAME=NSMO Dashboard
set R_VERSION=R-4.5.3
set PORT=6734

:: ── Resolve paths ────────────────────────────────────────────
set "ROOT_DIR=%~dp0"
if "%ROOT_DIR:~-1%"=="\" set "ROOT_DIR=%ROOT_DIR:~0,-1%"

set "RSCRIPT=%ROOT_DIR%\%R_VERSION%\bin\Rscript.exe"
set "APP_DIR=%ROOT_DIR%\app"
set "LIB_DIR=%ROOT_DIR%\%R_VERSION%\library"

:: ── Block .Rprofile ──────────────────────────────────────────
set "R_PROFILE_USER=NUL"

:: ── Basic checks ─────────────────────────────────────────────
if not exist "%RSCRIPT%" (
    echo ERROR: R not found. Contact Saurabh Datta.
    pause
    exit /b 1
)

if not exist "%APP_DIR%" (
    echo ERROR: App folder not found. Contact Saurabh Datta.
    pause
    exit /b 1
)

:: ── Port check ───────────────────────────────────────────────
netstat -ano | findstr ":%PORT% " >nul 2>&1
if %errorlevel%==0 (
    set /a PORT=%PORT%+1
    echo Port busy. Switching to !PORT!...
)

:: ── Convert backslashes to forward slashes for R ─────────────
set "LIB_DIR_R=%LIB_DIR:\=/%"
set "APP_DIR_R=%APP_DIR:\=/%"

:: ── Launch ───────────────────────────────────────────────────
echo.
echo ============================================================
echo   %APP_NAME% v8
echo ============================================================
echo.
echo   User :  %USERNAME%
echo   Port :  %PORT%
echo   From :  %ROOT_DIR%
echo.
echo   Browser opens in 20-30 seconds.
echo   DO NOT close this window while using the dashboard.
echo ============================================================
echo.

set "LAUNCH_SCRIPT=%TEMP%\nsmo_%USERNAME%_%RANDOM%.R"

(
    echo .libPaths^("%LIB_DIR_R%"^)
    echo shiny::runApp^(
    echo   appDir         = "%APP_DIR_R%",
    echo   port           = %PORT%,
    echo   host           = "127.0.0.1",
    echo   launch.browser = TRUE
    echo ^)
) > "%LAUNCH_SCRIPT%"

"%RSCRIPT%" "%LAUNCH_SCRIPT%"

if exist "%LAUNCH_SCRIPT%" del "%LAUNCH_SCRIPT%"

echo.
echo ============================================================
echo   Dashboard stopped. Press any key to close.
echo ============================================================
pause >nul
exit /b 0