@echo off
set SCRIPT_DIR=%~dp0
set ANT_OPTS=-Dsbt.ivy.home=C:/Users/%USERNAME%/.ivy2/

chcp 1253 && java %SBT_OPTS% -Xmx1024m -Dsbt.ivy.home=C:/Users/%USERNAME%/.ivy2/ -Ddb.dev.user=clarifidev -Ddb.dev.password=sti11f!y -Dfile.encoding=Cp1253 -Xms512M -Xmx1536M -Xss5M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M -jar "%SCRIPT_DIR%sbt-launch.jar" %* && chcp 437
