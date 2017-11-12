# Literal DataArray's and PooledDataArray's

Base Julia allows uses to specify literal arrays using a syntax
based on Matlab. Vectors are written as a set of values, separated by
commas and enclosed by square brackets:

    [x_1, x_2, ..., x_n]

In contrast, matrices are written as a set of rows. The values in each
row are separated by whitespace and terminate in a semicolon.
In addition to the formatting of rows, the matrix as a whole is enclosed
in square brackets:

    [x_11 x_12 ... x_1n;
     x_21 x_22 ... x_2n;
     x_m1 x_m2 ... x_mn]

Julia's parser rewrites both of these literals as calls to the `vcat`
function. The `vcat` function computes the tightest type that would
enclose all of the values in the literal array. (REVISE)

Two macros, `@data` and `@pdata`, rewrite array literals into a form
that will allow direct construction of `DataArray`s and `PooledDataArray`s.

# Basic Principle

The basic mechanism that powers the `@data` and `@pdata` macros is the
rewriting of array literals as a call to DataArray or PooledDataArray
with a rewritten array literal and a Boolean mask that specifies where
`missing` occurred in the original literal.

For example,

    @data [1, 2, missing, 4]

will be rewritten as,

    DataArray([1, 2, 1, 4], [false, false, true, false])

Note the added `1` created during the rewriting of the array literal.
This value is called a `stub` and is always the first value found
in the literal array that is not `missing`. The use of stubs explains two
important properties of the `@data` and `@pdata` macros:

* If the entries of the array literal are not fixed values, but function calls, these function calls must be pure. Otherwise the impure funcion may be called more times than expected.
* It is not possible to specify a literal DataArray that contains only `missing` values.
* None of the variables used in a literal array can be called `missing`. This is just good style anyway, so it is not much of a limitation.

# Limitations

We restate the limitations noted above:

* If the entries of the array literal are not fixed values, but function calls, these function calls must be pure. Otherwise the impure funcion may be called more times than expected.
* It is not possible to specify a literal DataArray that contains only `missing` values.
* None of the variables used in a literal array can be called `missing`. This is just good style anyway, so it is not much of a limitation.


Note that the latter limitation is not very important, because a DataArray
with only `missing` values is already problematic because it has no well-defined
type in Julia.

One final limitation is that the rewriting rules are not able to
process a vector of vectors, as in the normal array literal:

    [ones(3), ones(3)]

The problem with these kinds of expressions is that their length
cannot be determined at parse-time, but must wait until run-time --
which is too late for the rewriting rule to construct the proper
Boolean mask.
