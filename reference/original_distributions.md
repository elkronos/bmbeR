# Registry of prior generators

`original_distributions` is the built-in, named list of generator
functions (see
[prior_generators](https://elkronos.github.io/bmbeR/reference/prior_generators.md)).
`distributions` is an alias kept for backwards compatibility. Both are
constants: to extend the registry, create your own copy with
[`add_distribution()`](https://elkronos.github.io/bmbeR/reference/add_distribution.md)
and pass it to
[`get_prior_distribution()`](https://elkronos.github.io/bmbeR/reference/add_distribution.md).

## Usage

``` r
original_distributions

distributions
```

## Format

A named list of functions.

An object of class `list` of length 10.

## Examples

``` r
names(original_distributions)
#>  [1] "student_t" "normal"    "cauchy"    "uniform"   "beta"      "gamma"    
#>  [7] "binomial"  "poisson"   "lognormal" "bernoulli"
```
