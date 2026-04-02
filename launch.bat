@echo off
setlocal EnableDelayedExpansion

:: ============================================================
:: NSMO Dashboard Launcher
:: ============================================================

:: --- Config (edit these two lines only) ---
set APP_NAME=NSMO_Dashboard
set PORT=6734

:: --- Resolve script directory (handles spaces in path) ---
set "ROOT_DIR=%~dp0"
:: Remove trailing backslash
if "%ROOT_DIR:~-1%"=="\" set "ROOT_DIR=%ROOT_DIR:~0,-1%"

:: --- Paths ---
set "RSCRIPT=%ROOT_DIR%\R-4.x.x\bin\Rscript.exe"
set "APP_DIR=%ROOT_DIR%\app"

:: ============================================================
:: CHECKS
:: ============================================================

:: Check Rscript exists
if not exist "%RSCRIPT%" (
    echo.
    echo ERROR: R not found at:
    echo   %RSCRIPT%
    echo.
    echo Please contact your dashboard administrator.
    pause
    exit /b 1
)

:: Check app folder exists
if not exist "%APP_DIR%" (
    echo.
    echo ERROR: App folder not found at:
    echo   %APP_DIR%
    echo.
    echo Please make sure you copied the full NSMO_Dashboard folder.
    pause
    exit /b 1
)

:: Check if port is already in use
netstat -ano | findstr ":%PORT% " >nul 2>&1
if %errorlevel%==0 (
    echo.
    echo WARNING: Port %PORT% is already in use.
    echo Trying port 6735 instead...
    set PORT=6735
)

:: ============================================================
:: LAUNCH
:: ============================================================

echo.
echo ============================================================
echo   NSMO Dashboard
echo ============================================================
echo   Starting on port %PORT%...
echo   This window must stay open while the app is running.
echo   Close this window to stop the app.
echo ============================================================
echo.

:: Use a temp file to pass paths safely (avoids quote/escape issues)
set "LAUNCH_SCRIPT=%TEMP%\nsmo_launch.R"

echo shiny::runApp( > "%LAUNCH_SCRIPT%"
echo   appDir  = r"(%APP_DIR%)", >> "%LAUNCH_SCRIPT%"
echo   port    = %PORT%, >> "%LAUNCH_SCRIPT%"
echo   host    = "127.0.0.1", >> "%LAUNCH_SCRIPT%"
echo   launch.browser = TRUE >> "%LAUNCH_SCRIPT%"
echo ) >> "%LAUNCH_SCRIPT%"

"%RSCRIPT%" "%LAUNCH_SCRIPT%"

:: ============================================================
:: CLEANUP
:: ============================================================

if exist "%LAUNCH_SCRIPT%" del "%LAUNCH_SCRIPT%"

echo.
echo Dashboard stopped. Press any key to close.
pause >nul
exit /b 0