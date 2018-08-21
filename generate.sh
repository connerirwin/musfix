#!/bin/bash

stack build &&

TEST_RUN='stack exec -- musfix --verbose test/*/*.smt2'
${TEST_RUN} &> correct_program_output
