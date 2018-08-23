#!/bin/bash
# Compares the output against the rubric

stack build &&

TEST_RUN='stack exec -- musfix --verbose test/*/*.smt2'
diff <(${TEST_RUN}) correct_program_output | vim -R -
