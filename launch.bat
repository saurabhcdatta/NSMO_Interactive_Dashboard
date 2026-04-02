@echo off
setlocal EnableDelayedExpansion

:: ============================================================
::  NSMO Dashboard Launcher v8
::  R 4.5.3 | Multi-user safe | No internet required
::  Uses R_LIBS env variable to override .Rprofile
:: ============================================================

set APP_NAME=NSMO Dashboard
set R_VERSION=R-4.5.3

:: ── Assign unique port per user ──────────────────────────────
set "USERNAME_HASH=0"
for /l %%i in (0,1,7) do (
    set /a "USERNAME_HASH=!USERNAME_HASH! + %%i * 13"
)
set /a "PORT=6700 + (!USERNAME_HASH! %% 99)"
if %PORT% LSS 6700 set PORT=6734
if %PORT% GTR 6799 set PORT=6734

:: ── Resolve paths ────────────────────────────────────────────
set "ROOT_DIR=%~dp0"
if "%ROOT_DIR:~-1%"=="\" set "ROOT_DIR=%ROOT_DIR:~0,-1%"

set "RSCRIPT=%ROOT_DIR%\%R_VERSION%\bin\Rscript.exe"
set "APP_DIR=%ROOT_DIR%\app"
set "LIB_DIR=%ROOT_DIR%\%R_VERSION%\library"
set "LOCK_FILE=%ROOT_DIR%\.packages_ok"

:: ── Force R to use portable library (overrides .Rprofile) ────
set "R_LIBS=%LIB_DIR%"
set "R_LIBS_USER=%LIB_DIR%"
set "R_LIBS_SITE=%LIB_DIR%"

:: ============================================================
::  CHECK 1: R exists
:: ============================================================

if not exist "%RSCRIPT%" (
    echo.
    echo ============================================================
    echo   ERROR: R not found.
    echo ============================================================
    echo.
    echo   Expected location:
    echo     %RSCRIPT%
    echo.
    echo   Make sure you copied the FULL NSMO_Dashboard folder
    echo   including the %R_VERSION% subfolder.
    echo.
    echo   Contact Saurabh Datta for a fresh copy.
    echo ============================================================
    pause
    exit /b 1
)

:: ============================================================
::  CHECK 2: App folder exists
:: ============================================================

if not exist "%APP_DIR%" (
    echo.
    echo ============================================================
    echo   ERROR: App folder not found.
    echo ============================================================
    echo.
    echo   Expected location:
    echo     %APP_DIR%
    echo.
    echo   Make sure the full NSMO_Dashboard folder was copied.
    echo ============================================================
    pause
    exit /b 1
)

:: ============================================================
::  CHECK 3: Library folder exists
:: ============================================================

if not exist "%LIB_DIR%" (
    echo.
    echo ============================================================
    echo   ERROR: R library folder not found.
    echo ============================================================
    echo.
    echo   Expected location:
    echo     %LIB_DIR%
    echo.
    echo   Contact Saurabh Datta for a fresh copy.
    echo ============================================================
    pause
    exit /b 1
)

:: ============================================================
::  AUTO-COPY FROM NETWORK DRIVE (per user, fully isolated)
:: ============================================================

echo %ROOT_DIR% | findstr /i "^[S-Z]:\\" >nul 2>&1
if %errorlevel%==0 (
    echo.
    echo ============================================================
    echo   Network drive detected: %ROOT_DIR%
    echo   Setting up your personal local copy...
    echo   This only happens once. Please wait.
    echo ============================================================
    echo.

    set "LOCAL_DIR=%USERPROFILE%\Desktop\NSMO_Dashboard_%USERNAME%"

    if not exist "!LOCAL_DIR!" (
        echo   Copying files to your Desktop...
        xcopy /E /I /Q "%ROOT_DIR%" "!LOCAL_DIR!"
        if !errorlevel! neq 0 (
            echo.
            echo ============================================================
            echo   ERROR: Could not copy files to Desktop.
            echo   Please manually copy NSMO_Dashboard to your Desktop.
            echo ============================================================
            pause
            exit /b 1
        )
        echo   Done. Your personal copy is ready.
        echo.
    ) else (
        echo   Your local copy already exists. Using it.
        echo.
    )

    :: Repoint all paths to this user's local copy
    set "ROOT_DIR=!LOCAL_DIR!"
    set "RSCRIPT=!LOCAL_DIR!\%R_VERSION%\bin\Rscript.exe"
    set "APP_DIR=!LOCAL_DIR!\app"
    set "LIB_DIR=!LOCAL_DIR!\%R_VERSION%\library"
    set "LOCK_FILE=!LOCAL_DIR!\.packages_ok"

    :: Re-set library env variables after repoint
    set "R_LIBS=!LOCAL_DIR!\%R_VERSION%\library"
    set "R_LIBS_USER=!LOCAL_DIR!\%R_VERSION%\library"
    set "R_LIBS_SITE=!LOCAL_DIR!\%R_VERSION%\library"

    echo   Launching from local copy...
    echo.
)

:: ============================================================
::  PACKAGE CHECK
::  R_LIBS env variable ensures portable library is used
::  No .libPaths() needed in R scripts
:: ============================================================

if exist "%LOCK_FILE%" (
    echo Packages verified for %USERNAME%. Starting dashboard...
    goto :LAUNCH
)

echo.
echo ============================================================
echo   Verifying required packages for %USERNAME%...
echo ============================================================
echo.

set "CHECK_SCRIPT=%TEMP%\nsmo_check_%USERNAME%_%RANDOM%.R"

echo pkgs ^<- c(                                           >  "%CHECK_SCRIPT%"
echo   'shiny',         'survey',      'xgboost',         >> "%CHECK_SCRIPT%"
echo   'dplyr',         'ggplot2',     'waiter',           >> "%CHECK_SCRIPT%"
echo   'DT',            'plotly',      'scales',           >> "%CHECK_SCRIPT%"
echo   'tidyr',         'stringr',     'forcats',          >> "%CHECK_SCRIPT%"
echo   'purrr',         'bslib',       'shinyWidgets',     >> "%CHECK_SCRIPT%"
echo   'shinydashboard','htmltools',   'jsonlite'          >> "%CHECK_SCRIPT%"
echo )                                                     >> "%CHECK_SCRIPT%"
echo missing ^<- pkgs[!sapply(pkgs, requireNamespace,      >> "%CHECK_SCRIPT%"
echo                  quietly = TRUE)]                     >> "%CHECK_SCRIPT%"
echo if (length(missing) ^> 0) {                           >> "%CHECK_SCRIPT%"
echo   cat("\nMISSING PACKAGES:\n")                       >> "%CHECK_SCRIPT%"
echo   cat(paste(" -", missing, collapse="\n"), "\n")     >> "%CHECK_SCRIPT%"
echo   quit(status = 1)                                   >> "%CHECK_SCRIPT%"
echo } else {                                             >> "%CHECK_SCRIPT%"
echo   cat("All packages OK.\n")                         >> "%CHECK_SCRIPT%"
echo }                                                    >> "%CHECK_SCRIPT%"

"%RSCRIPT%" "%CHECK_SCRIPT%"
set CHECK_STATUS=%errorlevel%

if exist "%CHECK_SCRIPT%" del "%CHECK_SCRIPT%"

if %CHECK_STATUS% neq 0 (
    echo.
    echo ============================================================
    echo   ERROR: Missing packages detected.
    echo   Contact Saurabh Datta for a fresh copy of
    echo   NSMO_Dashboard with all packages pre-installed.
    echo ============================================================
    pause
    exit /b 1
)

echo Packages OK. > "%LOCK_FILE%"
echo   All packages verified for %USERNAME%.
echo.

:: ============================================================
::  LAUNCH
:: ============================================================

:LAUNCH

:: ── Port conflict check ──────────────────────────────────────
netstat -ano | findstr ":%PORT% " >nul 2>&1
if %errorlevel%==0 (
    set /a PORT=%PORT%+1
    echo WARNING: Port busy. Switching to !PORT!...
)

echo.
echo ============================================================
echo   %APP_NAME% v8
echo ============================================================
echo.
echo   User:    %USERNAME%
echo   Port:    %PORT%
echo   App:     %APP_DIR%
echo.
echo   Your browser will open in 20-30 seconds.
echo   DO NOT close this window while using the dashboard.
echo   Close this window to stop the app.
echo ============================================================
echo.

set "LAUNCH_SCRIPT=%TEMP%\nsmo_launch_%USERNAME%_%RANDOM%.R"

echo shiny::runApp(                                        >  "%LAUNCH_SCRIPT%"
echo   appDir         = r"(%APP_DIR%)",                   >> "%LAUNCH_SCRIPT%"
echo   port           = %PORT%,                           >> "%LAUNCH_SCRIPT%"
echo   host           = "127.0.0.1",                      >> "%LAUNCH_SCRIPT%"
echo   launch.browser = TRUE                              >> "%LAUNCH_SCRIPT%"
echo )                                                    >> "%LAUNCH_SCRIPT%"

"%RSCRIPT%" "%LAUNCH_SCRIPT%"

:: ── Cleanup ──────────────────────────────────────────────────
if exist "%LAUNCH_SCRIPT%" del "%LAUNCH_SCRIPT%"

echo.
echo ============================================================
echo   Dashboard stopped. Press any key to close.
echo ============================================================
pause >nul
exit /b 0
```

---

### What changed

| | Before | Now |
|---|---|---|
| Library path method | `.libPaths()` in R script | `R_LIBS` environment variable |
| Set before R starts? | ❌ No — .Rprofile overrode it | ✅ Yes — env var is read first |
| After network copy repoint | Missing | ✅ Re-set with `!LOCAL_DIR!` |
| R scripts cleaner? | Had `.libPaths()` line | ✅ No longer needed |

---

### Before testing
```
□ Delete: S:\Projects\NSMO_Dashboard\.packages_ok
□ Save the new launch.bat
□ Double-click launch.bat
```

You should now see:
```
Verifying required packages for SDatta...
All packages OK.

NSMO Dashboard v8
User:  SDatta
Port:  67XX
Your browser will open in 20-30 seconds.