#!/bin/bash

CLASS_NAME="org.allenai.extraction.service.HttpServer"

# Confirm that swipl is installed, and set the java system property to load it.
if ! ( which swipl > /dev/null ); then
  # Note that this will prevent a running server from being restarted if swipl
  # disappears.
  echo "ERROR: swipl not found on path; exiting."
  exit 1
fi

eval `swipl --dump-runtime-variables`
JPL_BASE="${PLBASE}/lib/${PLARCH}"
# Check for the linux / EC2 location for the SWI libs. If it doesn't exist, try the OS X location.
if [[ -e "$JPL_BASE/libjpl.so" ]]; then
  # We need to set LD_LIBRARY_PATH and LD_PRELOAD if we're in Linux.
  export LD_LIBRARY_PATH="$JPL_BASE:/usr/lib/jvm/java/jre/lib/amd64:/usr/lib/jvm/java/jre/lib/amd64/server"
  export LD_PRELOAD="$JPL_BASE/libjpl.so"
elif [[ ! -e "$JPL_BASE/libjpl.jnilib" ]]; then
  echo "Couldn't find JPL libraries; exiting."
  exit 1
fi

export JVM_ARGS="-Xms3G -Xmx3G -Djava.library.path=$JPL_BASE"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`
. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
