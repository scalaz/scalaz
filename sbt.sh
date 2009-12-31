#!/bin/bash

java $SBT_OPTS -Dfile.encoding=UTF-8 -Xmx512M -jar `dirname $0`/xsbt-launch-0.6.8.jar "$@"
