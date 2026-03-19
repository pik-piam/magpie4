# costs

reads costs entering the objective function from a MAgPIE gdx file

## Usage

``` r
costs(gdx, file = NULL, level = "reg", type = "annuity", sum = TRUE)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- type:

  either "annuity" (as it enters the objetive function) or "investment"
  (investment)

- sum:

  total costs (TRUE) or detailed costs (FALSE)

## Value

A MAgPIE object containing the goal function costs including investments
\[million US\$17\]

## Author

Jan Philipp Dietrich, Markus Bonsch, Misko Stevanovic, Florian
Humpenoeder, Edna J. Molina Bacca, Michael Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
x <- costs(gdx)
} # }
```
