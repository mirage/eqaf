# Eq(af) - Constant time equal function

From some crypto libraries like [digestif][digestif], it needed to have a
constant time equal function to avoid timing attacks. To avoid replication of
code and ensure maintainability of this kind of function, we decide to provide a
little package which implements `equal` function on `string`.

This package, if `cstruct` or `base-bigarray` is available, will make this
`equal` function for them too (as `eqaf.cstruct` and `eqaf.bigarray`).

## Check tool

The main purpose of `eqaf` is to test the constant time execution of the equal
function. About that, the distribution provide a `check` tool which will compute
several times how many times we need to execute the equal function on different
inputs and equal inputs.

Then, by a linear regression, we compare results and expect that we did not have
any difference: the regression coefficient should be close to `0.0`.

You can test `eqaf` with this:

```sh
$ dune exec check/check.exe
```

This tool does not have any dependencies to be sure that `eqaf` is
self-contained.

[digestif]: https://github.com/mirage/digestif.git
