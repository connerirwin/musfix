#!/bin/bash
# Runs musfix with specific parameters

stack build &&
stack exec -- musfix --verbose "$@"

