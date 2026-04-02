@echo off
setlocal EnableDelayedExpansion

:: ============================================================
::  NSMO Dashboard Launcher v8
::  Clean version — runs in place, soft package warning only
:: ============================================================

set APP_NAME=NSMO Dashboard
set R_VERSION=R-4.5.3
set PORT=6734

:: ── Resolve where launch.bat is sitting ──────────────────────
set "ROOT_DIR=%~dp0"
if "%ROOT_DIR:~-1%"=="\" set "ROOT_DIR=%ROOT_DIR:~0,-1%"

set "RSCRIPT=%ROOT_DIR%\%R_VERSION%\bin\Rscript.exe"
set "APP_DIR=%ROOT_DIR%\app"
set "LIB_DIR=%ROOT_DIR%\%R_VERSION%\library"

:: ── Tell R exactly where packages are ────────────────────────
set "R_LIBS=%LIB_DIR%"
set "R_LIBS_USER=%LIB_DIR%"
set "R_LIBS_SITE=%LIB_DIR%"
set "R_PROFILE_USER=NUL"

:: ============================================================
::  STEP 1: BASIC CHECKS
:: ============================================================

if not exist "%RSCRIPT%" (
    echo.
    echo ============================================================
    echo   ERROR: R not found at:
    echo   %RSCRIPT%
    echo.
    echo   Make sure the %R_VERSION% folder is present.
    echo   Contact Saurabh Datta for a fresh copy.
    echo ============================================================
    pause
    exit /b 1
)

if not exist "%APP_DIR%" (
    echo.
    echo ============================================================
    echo   ERROR: App folder not found at:
    echo   %APP_DIR%
    echo.
    echo   Make sure the app folder is present.
    echo   Contact Saurabh Datta for a fresh copy.
    echo ============================================================
    pause
    exit /b 1
)

:: ============================================================
::  STEP 2: SOFT PACKAGE CHECK (warns but does not block)
:: ============================================================

echo.
echo   Checking packages...

set "CHECK_SCRIPT=%TEMP%\nsmo_check_%USERNAME%_%RANDOM%.R"

(
    echo pkgs ^<- c^(
    echo   'shiny', 'survey', 'xgboost',
    echo   'dplyr', 'ggplot2', 'waiter',
    echo   'DT', 'plotly', 'scales',
    echo   'tidyr', 'stringr', 'forcats',
    echo   'purrr', 'bslib', 'shinyWidgets',
    echo   'shinydashboard', 'htmltools', 'jsonlite'
    echo ^)
    echo missing ^<- pkgs[!sapply^(pkgs, requireNamespace, quietly=TRUE^)]
    echo if ^(length^(missing^) ^> 0^) ^{
    echo   cat^("WARNING: These packages not found:\n"^)
    echo   cat^(paste^(" -", missing, collapse="\n"^), "\n"^)
    echo   cat^("Dashboard will attempt to launch anyway.\n"^)
    echo ^} else ^{
    echo   cat^("All packages OK.\n"^)
    echo ^}
) > "%CHECK_SCRIPT%"

"%RSCRIPT%" "%CHECK_SCRIPT%"
if exist "%CHECK_SCRIPT%" del "%CHECK_SCRIPT%"

:: ============================================================
::  STEP 3: PORT CHECK
:: ============================================================

netstat -ano | findstr ":%PORT% " >nul 2>&1
if %errorlevel%==0 (
    set /a PORT=%PORT%+1
    echo   Port busy — switching to !PORT!...
)

:: ============================================================
::  STEP 4: LAUNCH
:: ============================================================

echo.
echo ============================================================
echo   %APP_NAME% v8
echo ============================================================
echo.
echo   User :  %USERNAME%
echo   Port :  %PORT%
echo   From :  %ROOT_DIR%
echo.
echo   Starting up — browser opens in 20-30 seconds.
echo   DO NOT close this window while using the dashboard.
echo ============================================================
echo.

set "LAUNCH_SCRIPT=%TEMP%\nsmo_%USERNAME%_%RANDOM%.R"

(
    echo .libPaths^("%LIB_DIR%"^)
    echo shiny::runApp^(
    echo   appDir         = "%APP_DIR%",
    echo   port           = %PORT%,
    echo   host           = "127.0.0.1",
    echo   launch.browser = TRUE
    echo ^)
) > "%LAUNCH_SCRIPT%"

"%RSCRIPT%" "%LAUNCH_SCRIPT%"

:: ── Cleanup ──────────────────────────────────────────────────
if exist "%LAUNCH_SCRIPT!" del "%LAUNCH_SCRIPT%"

echo.
echo ============================================================
echo   Dashboard stopped. Press any key to close.
echo ============================================================
pause >nul
exit /b 0