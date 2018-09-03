#!/bin/bash
# Compares the output against the rubric

stack build --fast &&

stack exec -- musfix -o tmp_verification_file --silent --verbose test/**/*.smt2
git diff --no-index --word-diff --patience --color correct_program_output tmp_verification_file
rm tmp_verification_file
