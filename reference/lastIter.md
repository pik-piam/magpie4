# lastIter

Returns the value of a parameter in the last iteration

## Usage

``` r
lastIter(gdx, param, MagpieOutput = TRUE)
```

## Arguments

- gdx:

  GDX file

- param:

  Parameter to be returned

- MagpieOutput:

  In inelastic model runs, the food demand model is run once in iter1
  based on exo prices prescribed in iter0, then the magpie model in
  iter1. The magpie results therefore appear in iter1. In elastic model
  runs, iter2 is started by rerunning the food demand model, but if
  convergence is reached, MAgPIE does not run again. MAgPIE results are
  therefore still in iter1, even if the iteration is in iter2.

## Value

magpie object

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
x <- lastIter(gdx)
} # }
```
