#!/bin/bash

java $SBT_OPTS -Xmx512M -jar `dirname $0`/sbt-launcher.jar "$@"