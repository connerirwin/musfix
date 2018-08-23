#!/bin/bash
# Compares the output against the rubric

stack build &&

TEST_RUN='stack exec -- musfix --verbose test/**/*.smt2'
${TEST_RUN} &> tmp_verification_file
git diff --no-index --word-diff --patience --color tmp_verification_file correct_program_output
rm tmp_verification_file
