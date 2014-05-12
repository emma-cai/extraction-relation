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
# Check for the OS X JPL. If it doesn't exist, try the EC2 location.
if [[ ! -e "$JPL_BASE/libjpl.jnilib" && ! -e "$JPL_BASE/libjpl.so" ]]; then
  echo "Couldn't find JPL libraries; exiting."
  exit 1
fi

export JVM_ARGS="-Xms3G -Xmx3G -Djava.library.path=$JPL_BASE"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`
. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
