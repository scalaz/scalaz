set SCRIPT_DIR=%~dp0
java %SBT_OPTS% -Dfile.encoding=UTF-8 -Xmx512M -jar "%SCRIPT_DIR%xsbt-launch-0.6.8.jar" %*