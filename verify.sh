#!/bin/bash
# Compares the output against the rubric

TEST_RUN='stack exec -- musfix --verbose test/*.smt2'
diff <(${TEST_RUN}) rubric
