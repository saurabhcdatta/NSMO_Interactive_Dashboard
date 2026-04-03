@echo off
setlocal EnableDelayedExpansion

:: ============================================================
::  NSMO Dashboard Launcher v8 — Final with Timer
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
echo   From :  %ROOT_DIR%
echo.
echo   Loading dashboard. This takes 1-3 minutes.
echo   Watch the timer below. Browser opens automatically.
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

:: Format seconds with leading zero
set "SECS_FMT=0%SECS%"
set "SECS_FMT=!SECS_FMT:~-2!"

:: Check if app is listening yet
netstat -ano | findstr ":%PORT% " | findstr "LISTENING" >nul 2>&1
if %errorlevel%==0 goto :READY

:: Print timer — one line per second
echo   Loading... %MINS%m !SECS_FMT!s

:: Safety timeout after 5 minutes
if %SECONDS% LSS 300 goto :TIMER_LOOP

:: ── Timeout warning ──────────────────────────────────────────
echo.
echo ============================================================
echo   WARNING: Taking longer than 5 minutes.
echo   Try opening manually: http://127.0.0.1:%PORT%
echo   Or contact Saurabh Datta.
echo ============================================================
goto :WAIT_FOR_EXIT

:: ── Ready ────────────────────────────────────────────────────
:READY
echo.
echo ============================================================
echo   Dashboard ready in %MINS%m !SECS_FMT!s
echo   Opening browser now...
echo   http://127.0.0.1:%PORT%
echo ============================================================
echo.
echo   Keep this window open while using the dashboard.
echo   Press any key to stop the dashboard and close.
echo ============================================================

start http://127.0.0.1:%PORT%

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