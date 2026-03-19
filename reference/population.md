# population

reads population out of a MAgPIE gdx file

## Usage

``` r
population(
  gdx,
  file = NULL,
  level = "reg",
  age = FALSE,
  sex = FALSE,
  bmi_groups = FALSE
)
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

- age:

  if TRUE, population is split up by age groups

- sex:

  if TRUE, population is split up by sex

- bmi_groups:

  if TRUE, the population will be split up in body-mass-index groups.

## Value

population as MAgPIE object (million people)

## See also

[`reportPopulation`](reportPopulation.md)

## Author

Florian Humpenoeder, Benjamin Bodirsky, Isabelle Weindl

## Examples

``` r
if (FALSE) { # \dontrun{
x <- population(gdx)
} # }
```
