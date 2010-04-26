set SCRIPT_DIR=%~dp0
java %SBT_OPTS% -Dfile.encoding=UTF-8 -Xmx512M -jar "%SCRIPT_DIR%sbt-launch-0.7.3" @sbt.boot.properties %*
