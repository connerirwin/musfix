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

The musfix smt format uses a lisp-like representation for expressions and other declarations. All expressions are therefore prefix. For example, to write the arithmetic expression `1 + 2`, you would write `(+ 1 2)`.

## Expression

An expression can consist of:

* Literal
  * Booleans: `True`, `False`
  * Integers: `1`, `-27`, `42`, etc.
  * Sets: `{}`, `(Set 1 -27 42)`, `(Set True)`, etc.
  * Maps: `(Map_default 7)`, etc.
* Variable: `v`, `something`, `jack_the_rabbit`, etc.
* Constant: `v'`, `somethingElse`, `patience`, etc.
* Function:
  * Arithmetic operations: `*`, `+`, `-`
  * Boolean operations: `=`, `!=`, `>`, `>=`, `<`, `<=`, `&&`, `||`, `=>`, `<=>`
  * Set operations: `union`, `intersection`, `diff`
  * Map operations: `member`, `subset`
  * Uninterpreted functions: `length`, `f`, etc.

Expressions are used to define the program logic that you wish to verify, either in Horn Constraints or in Qualifiers.


## Variable

Before a variable can be used in an expression, it must first be declared in the binding of the constraint or qualifier where it is being used.

To declare a variable, use a lower-case or underscore prefixed name followed by a sort

`(name Sort)`

Example:

```lisp
(i Int)
(b Bool)
```

## Sort

There are built-in sort primitives for `Int`, `Bool`, `Map`, and `Set`.

`Map` and `Set` both are constructed sorts that require additional sorts as parameters.

Example:

```lisp
(Map Int Int)
(Set Bool)
```

Additional sorts can be declared with the top-level binding `declare-sort` using an upper-case name and a count specifying the number of additional sort parameters.

`(declare-sort Name count)`

Example:

```lisp
(declare-sort List 1)

... (x (List Int)) ... ; Inside the binding of a constraint/qualifier
```

It is also possible to use polymorphic sorts anywhere that a sort is required by preceding a name with an @. This can also be done with constructed sorts.

`@Name`

Example:

```lisp
; All of these variables can be unified to have the same sort

(declare-sort List 1)

(x (List Int)) ; x is a List of Int

(y (List @0)) ; y is a List of anything

(z (@A @B)) ; z is any constructed sort that takes one sort
```


## Constants

Constants can be used in place of variables. To declare a constant, use a lower-case name followed by a sort.

`(declare-const name Sort)`

Constants can also be declared __distinct__, which ensures that they must be assigned different values

`(assert (distinct names))`

Example:

```lisp
(declare-const one Int)
(declare-const zero Int)

(assert (distinct one zero))
```

## Uninterpreted Functions

Functions can be declared with a lower-case name, a list of the sorts of arguments, and the return sort. When called, this function is uninterpreted, but will sort check its return type and arguments.

`(declare-fun name (Argument_Sorts) Return_Sort)`

Example:

```lisp
(declare-fun foo (Int Int) Bool)
(declare-fun bar (Int) Bool)

; Inside of a constraint/qualifier
... (v0 Int) (v1 Int) ... (= (foo v0 v1) (bar v1))
```

## Qualifiers

Qualifiers are a set of expressions that can be applied to the arguments of a well-formedness constraint. The horn-solver will then look for the strongest (this is the default option, though this can be configured to instead solve for weakest) set of these qualifiers that satisfy all constraints.

To declare a qualifier, use a name followed by a list of variables and an expression.

`(qualif name (variables) expression)`

Example:

```lisp
(qualif Eq ((v @a)(z @a)) (= v z)) ; This will accept any arguments of the same sort
```

## Well-formedness Constraints

A well-formedness constraint will cause the horn-solver to search for a set of qualifiers that can be assigned to the arguments while satisfying all constraints.

To declare a well-formedness constraint, use a $ prefixed name followed by a list of variables.

`(wf $name (variables))`

Example:

```lisp
(wf $k0 ((v0 Int)(v1 Int)))
```

## Horn Constraints

A horn constraint contains a logical implication that you wish to verify. Any number of horn constraints can be checked, and the solver will search for a simultaneous solution to all of them.

`(constraint (forall (variables) (=> expression expression)))`

[Example:](test/sample/gfp00.msmt)

```lisp
(qualif Pos ((v Int)) (<= 0 v))

(wf $k0 ((v0 Int)))

(constraint
  (forall ((v1 Int))
   (=> ($k0 v1)
       (< 0 (+ v1 1)))))
```

## Example Files

[Sorts, Functions, Qualifiers](test/sample/nadia.msmt)

[Distinct Constants](test/pos/lit00.msmt)


# Using Development Scripts

To run the program quickly

    run.sh [musfix_args]

To run the program in profiling mode (using -xc by default)

    profile.sh [musfix_args] +RTS [ghc_args]

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
