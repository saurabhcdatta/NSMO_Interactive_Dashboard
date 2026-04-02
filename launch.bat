@echo off
setlocal EnableDelayedExpansion

:: ============================================================
::  NSMO Dashboard Launcher v8
::  R 4.5.3 | Auto-installs packages | Auto-copies from network
:: ============================================================

set APP_NAME=NSMO Dashboard
set PORT=6734
set R_VERSION=R-4.5.3

:: ── Resolve paths ────────────────────────────────────────────
set "ROOT_DIR=%~dp0"
if "%ROOT_DIR:~-1%"=="\" set "ROOT_DIR=%ROOT_DIR:~0,-1%"

set "RSCRIPT=%ROOT_DIR%\%R_VERSION%\bin\Rscript.exe"
set "APP_DIR=%ROOT_DIR%\app"
set "LIB_DIR=%ROOT_DIR%\%R_VERSION%\library"
set "LOCK_FILE=%ROOT_DIR%\.packages_installed"

:: ── Check R exists ───────────────────────────────────────────
if not exist "%RSCRIPT%" (
    echo.
    echo ERROR: R not found. Make sure you copied the full
    echo NSMO_Dashboard folder including the %R_VERSION% subfolder.
    echo.
    echo Expected location:
    echo   %RSCRIPT%
    echo.
    echo Contact Saurabh Datta for a fresh copy.
    pause
    exit /b 1
)

:: ── Check app folder exists ──────────────────────────────────
if not exist "%APP_DIR%" (
    echo.
    echo ERROR: App folder not found at:
    echo   %APP_DIR%
    echo.
    echo Make sure the full NSMO_Dashboard folder was copied.
    pause
    exit /b 1
)

:: ============================================================
::  AUTO-COPY FROM NETWORK DRIVE
::  Detects network drives (S: Q: R: U: V:) and copies locally
::  to your Desktop for best performance
:: ============================================================

echo %ROOT_DIR% | findstr /i "^[S-Z]:\\" >nul 2>&1
if %errorlevel%==0 (
    echo.
    echo ============================================================
    echo   Network drive detected: %ROOT_DIR%
    echo   Copying to your Desktop for best performance...
    echo   This only happens once. Please wait.
    echo ============================================================
    echo.

    set "LOCAL_DIR=%USERPROFILE%\Desktop\NSMO_Dashboard_local"

    if not exist "!LOCAL_DIR!" (
        xcopy /E /I /Q "%ROOT_DIR%" "!LOCAL_DIR!"
        if !errorlevel! neq 0 (
            echo.
            echo ERROR: Could not copy files to Desktop.
            echo Please manually copy NSMO_Dashboard folder
            echo to your Desktop and run launch.bat from there.
            pause
            exit /b 1
        )
        echo Copy complete.
        echo.
    ) else (
        echo Local copy already exists. Using it.
        echo.
    )

    :: Repoint all paths to local copy
    set "ROOT_DIR=!LOCAL_DIR!"
    set "RSCRIPT=!LOCAL_DIR!\%R_VERSION%\bin\Rscript.exe"
    set "APP_DIR=!LOCAL_DIR!\app"
    set "LIB_DIR=!LOCAL_DIR!\%R_VERSION%\library"
    set "LOCK_FILE=!LOCAL_DIR!\.packages_installed"

    echo Launching from local copy for best performance...
    echo.
)

:: ============================================================
::  PACKAGE INSTALLATION (runs once only)
::  Skips if lock file exists from a previous successful install
:: ============================================================

if exist "%LOCK_FILE%" (
    echo Packages already installed. Skipping...
    goto :LAUNCH
)

echo.
echo ============================================================
echo   First-time setup: Installing required packages
echo   This will take 5-10 minutes. Please wait.
echo   Do not close this window.
echo ============================================================
echo.

set "INSTALL_SCRIPT=%TEMP%\nsmo_install_%RANDOM%.R"

echo pkgs ^<- c(                                           >  "%INSTALL_SCRIPT%"
echo   'shiny',        'survey',       'xgboost',         >> "%INSTALL_SCRIPT%"
echo   'dplyr',        'ggplot2',      'waiter',           >> "%INSTALL_SCRIPT%"
echo   'DT',           'plotly',       'scales',           >> "%INSTALL_SCRIPT%"
echo   'tidyr',        'stringr',      'forcats',          >> "%INSTALL_SCRIPT%"
echo   'purrr',        'bslib',        'shinyWidgets',     >> "%INSTALL_SCRIPT%"
echo   'shinydashboard','htmltools',   'jsonlite'          >> "%INSTALL_SCRIPT%"
echo )                                                     >> "%INSTALL_SCRIPT%"
echo.                                                      >> "%INSTALL_SCRIPT%"
echo missing ^<- pkgs[!sapply(pkgs, requireNamespace,      >> "%INSTALL_SCRIPT%"
echo                          quietly = TRUE)]             >> "%INSTALL_SCRIPT%"
echo.                                                      >> "%INSTALL_SCRIPT%"
echo if (length(missing) ^> 0) {                           >> "%INSTALL_SCRIPT%"
echo   cat("Installing:", paste(missing, collapse=", "),   >> "%INSTALL_SCRIPT%"
echo       "\n")                                           >> "%INSTALL_SCRIPT%"
echo   install.packages(                                   >> "%INSTALL_SCRIPT%"
echo     missing,                                          >> "%INSTALL_SCRIPT%"
echo     lib   = r"(%LIB_DIR%)",                           >> "%INSTALL_SCRIPT%"
echo     repos = "https://cran.r-project.org",             >> "%INSTALL_SCRIPT%"
echo     quiet = FALSE                                     >> "%INSTALL_SCRIPT%"
echo   )                                                   >> "%INSTALL_SCRIPT%"
echo } else {                                              >> "%INSTALL_SCRIPT%"
echo   cat("All packages already present.\n")             >> "%INSTALL_SCRIPT%"
echo }                                                     >> "%INSTALL_SCRIPT%"

:: Run install script
"%RSCRIPT%" "%INSTALL_SCRIPT%"
set INSTALL_STATUS=%errorlevel%

:: Cleanup
if exist "%INSTALL_SCRIPT%" del "%INSTALL_SCRIPT%"

:: Check install succeeded
if %INSTALL_STATUS% neq 0 (
    echo.
    echo ERROR: Package installation failed.
    echo Check your internet connection and try again.
    echo If the problem persists contact Saurabh Datta.
    echo.
    pause
    exit /b 1
)

:: Write lock file so install never runs again
echo Installed on %DATE% at %TIME% > "%LOCK_FILE%"

echo.
echo ============================================================
echo   Setup complete! Launching dashboard...
echo ============================================================
echo.

:: ============================================================
::  LAUNCH
:: ============================================================

:LAUNCH

:: ── Port check ───────────────────────────────────────────────
netstat -ano | findstr ":%PORT% " >nul 2>&1
if %errorlevel%==0 (
    echo WARNING: Port %PORT% in use, switching to 6735...
    set PORT=6735
)

echo.
echo ============================================================
echo   %APP_NAME% v8
echo ============================================================
echo.
echo   Starting on port %PORT%...
echo   Your browser will open in 20-30 seconds.
echo   DO NOT close this window while using the dashboard.
echo ============================================================
echo.

:: Write launch script to temp file
set "LAUNCH_SCRIPT=%TEMP%\nsmo_launch_%RANDOM%.R"

echo shiny::runApp(                         >  "%LAUNCH_SCRIPT%"
echo   appDir         = r"(%APP_DIR%)",     >> "%LAUNCH_SCRIPT%"
echo   port           = %PORT%,             >> "%LAUNCH_SCRIPT%"
echo   host           = "127.0.0.1",        >> "%LAUNCH_SCRIPT%"
echo   launch.browser = TRUE                >> "%LAUNCH_SCRIPT%"
echo )                                      >> "%LAUNCH_SCRIPT%"

"%RSCRIPT%" "%LAUNCH_SCRIPT%"

:: ── Cleanup ──────────────────────────────────────────────────
if exist "%LAUNCH_SCRIPT%" del "%LAUNCH_SCRIPT%"

echo.
echo Dashboard stopped. Press any key to close.
pause >nul
exit /b 0
```

---

### What this version does end to end
```
Launch from S: drive
       ↓
Detects network drive
       ↓
Copies NSMO_Dashboard → Desktop\NSMO_Dashboard_local  (once only)
       ↓
Checks packages installed (.packages_installed lock file)
       ↓
First run: installs all packages → writes lock file  (5-10 min, once only)
Subsequent runs: skips straight to launch
       ↓
Checks port 6734 → bumps to 6735 if busy
       ↓
Launches app → browser opens automatically
```

---

### Three things to do before sharing
```
□ Replace package list with your full v8 dependencies
□ Test on one colleague's machine first
□ Zip and upload to SharePoint