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
    echo.
    echo ============================================================
    echo   ERROR: R not found.
    echo   Contact Saurabh Datta for a fresh copy.
    echo ============================================================
    pause
    exit /b 1
)

if not exist "%APP_DIR%" (
    echo.
    echo ============================================================
    echo   ERROR: App folder not found.
    echo   Contact Saurabh Datta for a fresh copy.
    echo ============================================================
    pause
    exit /b 1
)

:: ── Port check ───────────────────────────────────────────────
netstat -ano | findstr ":%PORT% " >nul 2>&1
if %errorlevel%==0 (
    set /a PORT=%PORT%+1
    echo   Port busy. Switching to !PORT!...
)

:: ── Convert backslashes to forward slashes for R ─────────────
set "LIB_DIR_R=%LIB_DIR:\=/%"
set "APP_DIR_R=%APP_DIR:\=/%"

:: ── Header ───────────────────────────────────────────────────
echo.
echo ============================================================
echo   %APP_NAME% v8
echo ============================================================
echo.
echo   User :  %USERNAME%
echo   Port :  %PORT%
echo.
echo   Please wait while the dashboard loads.
echo   Your browser will open automatically when ready.
echo   DO NOT close this window while using the dashboard.
echo ============================================================
echo.

:: ── Write launch script ──────────────────────────────────────
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

:: ── Launch R in background ───────────────────────────────────
start /b "" "%RSCRIPT%" "%LAUNCH_SCRIPT%"

:: ── Timer loop ───────────────────────────────────────────────
set SECONDS=0

:TIMER_LOOP
timeout /t 1 /nobreak >nul
set /a SECONDS+=1
set /a MINS=SECONDS/60
set /a SECS=SECONDS%%60

set "SECS_FMT=0%SECS%"
set "SECS_FMT=!SECS_FMT:~-2!"

:: Check if app is ready
netstat -ano | findstr ":%PORT% " | findstr "LISTENING" >nul 2>&1
if %errorlevel%==0 goto :READY

:: Show spinner with timer
set /a SPIN=SECONDS%%4
if !SPIN!==0 echo   ^| Browser loading... %MINS%m !SECS_FMT!s
if !SPIN!==1 echo   / Browser loading... %MINS%m !SECS_FMT!s
if !SPIN!==2 echo   - Browser loading... %MINS%m !SECS_FMT!s
if !SPIN!==3 echo   \ Browser loading... %MINS%m !SECS_FMT!s

goto :TIMER_LOOP

:: ── Ready ────────────────────────────────────────────────────
:READY
echo.
echo ============================================================
echo   Dashboard ready in %MINS%m !SECS_FMT!s
echo   Your browser is opening now...
echo ============================================================
echo.
echo   Keep this window open while using the dashboard.
echo   Press any key when finished to close.
echo ============================================================

:: ── Wait for user to close ───────────────────────────────────
:WAIT_FOR_EXIT
pause >nul

:: ── Cleanup ──────────────────────────────────────────────────
if exist "%LAUNCH_SCRIPT%" del "%LAUNCH_SCRIPT%"

echo.
echo ============================================================
echo   Dashboard stopped. Goodbye!
echo ============================================================
timeout /t 2 /nobreak >nul
exit /b 0