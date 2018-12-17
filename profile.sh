#!/bin/bash
# Runs musfix profiling

PROFILE_FLAGS='--work-dir .stack-work-profile'
stack ${PROFILE_FLAGS} --profile build &&
stack ${PROFILE_FLAGS} exec -- musfix +RTS -xc -RTS "$@"

