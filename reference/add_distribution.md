# Manage and draw from a registry of prior generators

`add_distribution()` returns a copy of a registry with a new generator
added; `reset_distributions()` returns the built-in registry;
`get_prior_distribution()` draws from a named generator in a registry.

## Usage

``` r
add_distribution(distributions, name, func)

reset_distributions()

get_prior_distribution(
  dist_type,
  params,
  distributions = original_distributions
)
```

## Arguments

- distributions:

  A named list of generator functions, such as
  [original_distributions](https://elkronos.github.io/bmbeR/reference/original_distributions.md).

- name:

  Name for the new generator.

- func:

  A function whose first argument is the number of draws.

- dist_type:

  Name of the generator to use.

- params:

  A named list of arguments passed to the generator, including
  `sample_size`.

## Value

`add_distribution()` and `reset_distributions()` return a named list of
functions; `get_prior_distribution()` returns the draws.

## Details

The registry is an ordinary list, so these functions have no side
effects: keep the list returned by `add_distribution()` and pass it to
`get_prior_distribution()` via its `distributions` argument.

## Examples

``` r
my_dists <- add_distribution(original_distributions, "exponential",
                             function(sample_size, rate = 1) rexp(sample_size, rate))
get_prior_distribution("exponential", list(sample_size = 5, rate = 2),
                       distributions = my_dists)
#> [1] 1.1857219 0.3343330 0.1007609 0.8219809 2.5876548
```
