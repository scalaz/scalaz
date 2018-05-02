#!/bin/bash
hash=`git rev-parse HEAD`
sed -i -e "s/SNAPSHOT/$hash-SNAPSHOT/g" ./version.sbt
sbt publish
