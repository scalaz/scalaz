#!/bin/bash

java $SBT_OPTS -Dfile.encoding=UTF-8 -Xmx1024M -XX:MaxPermSize=128M -XX:NewSize=128M -XX:NewRatio=3 -jar `dirname $0`/xsbt-launch-0.6.10.jar "$@"
