set SCRIPT_DIR=%~dp0
java %SBT_OPTS% -Xmx512M -jar "%SCRIPT_DIR%xsbt-launch-0.6.8.jar" %*