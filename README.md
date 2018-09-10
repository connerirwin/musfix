# Musfix

General purpose version of Synquid's Horn clause solver.

# Building Musfix

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

2. Install the Z3 Theorem Prover v4.7.1

        git clone --branch z3-4.7.1 https://github.com/Z3Prover/z3.git
        cd z3
        python scripts/mk_make.py
        cd build; make
        sudo make install

   After building Z3 it may be necessary to update the system's library cache:

        sudo ldconfig

3. Install Musfix

        git clone https://github.com/connerirwin/musfix.git
        cd musfix
        stack build

4. Run Musfix

        stack exec -- musfix

# Command Line Options

Usage:

    musfix INPUT_FILES [-o|--output FILE] [-a|--append] [-s|--silent] [--verbose] [-l|--least-fixpoint] [--version]

Run a fixpoint solver on INPUT_FILES to find all solutions satisfying the constraints

Available options:

    -h,--help                Show this help text
    -o,--output FILE         Prints results to the specified file
    -a,--append              Append file output
    -s,--silent              Supresses command-line output
    --verbose                Output additional logging
    -l,--least-fixpoint      Use a least-fixpoint solver (default is greatest)
    --version                Show current version

# .msmt File Format

## Variable

To declare a variable, use a lower-case or underscore prefixed name followed by a sort

`(name Sort)`

Example:

    (i Int)
    (b Bool)

## Sort

There are built-in sort primitives for `Int`, `Bool`, `Map`, and `Set`.

`Map` and `Set` both are constructed sorts that require additional sorts are parameters.

Example:

    (Map Int Int)
    (Set Bool)

Additional sorts can be declared with an Upper-case name and a count specifying the number of additional sort parameters.

`(declare-sort Name count)`

Example:

    (declare-sort List 1)
    (x (List Int))

It is also possible to use polymorphic sorts anywhere that a sort is required by preceding a name with an @. This can also be done with constructed sorts.

`@Name`

Example:

    (declare-sort List 1)

    (x (List Int)) ; x is a List of Int

    (y (List @0)) ; y is a List of anything

    (z (@A @B)) ; z is any constructed sort that takes one sort

    ; All of these variables can be compared with one another, as they can be unified to have the same sort

## Expression

TODO

## Constants

Constants can be used in place of variables. To declare a constant, use a lower-case name followed by a sort.

`(declare-const name Sort)`

Constants can also be declared __distinct__, which ensures that they must be assigned different values

`(assert (distinct names))`

Example:

    (declare-const one Int)
    (declare-const zero Int)

    (assert (distinct one zero))

## Uninterpreted Functions

Functions can be declared with a lower-case name, a list of the sorts of arguments, and the return sort. When called, this function is uninterpreted, but will sort check its return type and arguments.

`(declare-fun name (Argument_Sorts) Return_Sort)`

Example:

    (declare-fun bar (Int) Bool)

    (v0 Int)
    (= (bar v0) True)

## Qualifiers

Qualifiers are a set of expressions that can be applied to the arguments of a well-formedness constraint. The horn-solver will then look for the strongest (this is the default option, though this can be configured to instead solve for weakest) set of these qualifiers that satisfy all constraints.

To declare a qualifier, use a name followed by a list of variables and an expression.

`(qualif name (variables) expression)`

Example:

    (qualif Eq ((v @a)(z @a)) (= v z)) ; This will accept any arguments of the same sort

## Well-formedness Constraints

A well-formedness constraint ensures that all of the arguments can be assigned some set of qualifiers. TODO

To declare a well-formedness constraint, use a $ prefixed name followed by a list of variables

`(wf $name (variables))`

Example:

    (wf $k0 ((v0 Int)(v1 Int)))

## Horn Constraints
 TODO
Logical implication, the first expression implies the second.

`(constraint (forall (variables) (=> expression expression)))`

[Example:](test/sample/gfp00.msmt)

    (qualif Pos   ((v Int)) (<= 0 v))

    (wf $k0 ((v0 Int)))

    (constraint
      (forall ((v1 Int))
       (=> ($k0 v1)
           (< 0 (+ v1 1)))))


## Example Files

[Sorts, Functions, Qualifiers](test/sample/nadia.msmt)

[Distinct Constants](test/pos/lit00.msmt)


# Using Development Scripts

To run the program quickly

    run.sh [musfix args]

To run the program in profiling mode.

    profile.sh [musfix args] +RTS [ghc args]

Example ghc arguments:
* -p &nbsp;&nbsp;&nbsp; Generates musfix.prof containing time and allocation statistics
* -xc &nbsp;&nbsp; Dumps a stack trace when an exception is raised

A full list can be found [here](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)

# Integration Testing

To run the integration tests

    stack test

To accept changes to the test suite and regenerate the template files

    TASTY_ACCEPT=true stack test

To verify changes to the tests

    git diff --word-diff --patience --color

# Debugging in GHCi

To set a breakpoint

    :break identifier
    :break [module] line [column]

To set program arguments

    :set args arguments_to_main

Then run with tracing enabled

    :trace main

More details [here](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/ghci-debugger.html)
