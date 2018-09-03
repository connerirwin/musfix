#!/bin/bash

stack build --fast &&
stack exec -- musfix -o correct_program_output --silent --verbose test/**/*.smt2
