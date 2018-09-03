#!/bin/bash
# Runs musfix with specific parameters

stack build --fast &&
stack exec -- musfix --verbose "$@"

