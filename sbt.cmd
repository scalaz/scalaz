@echo off
set SCRIPT_DIR=%~dp0
if defined PROXY_HOST set PROXY_OPTS=-Dhttp.proxyHost=%PROXY_HOST% -Dhttp.proxyPort=%PROXY_PORT% -Dhttp.proxyUser=%PROXY_USER% -Dhttp.proxyPassword=%PROXY_PASSWORD%
java %SBT_OPTS% %PROXY_OPTS% -Dfile.encoding=UTF-8 -Xss4M -Xmx1024M -XX:MaxPermSize=256M -XX:NewSize=128M -XX:NewRatio=3 -jar "%SCRIPT_DIR%sbt-launch-0.7.7.jar" @sbt.boot.properties %*
