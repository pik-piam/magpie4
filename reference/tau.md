# tau

Calculates Landuse intensity indicator tau based on a MAgPIE gdx file

## Usage

``` r
tau(
  gdx,
  file = NULL,
  level = "reg",
  start_value = FALSE,
  digits = 4,
  prev_year = "y1985",
  type = "crop"
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
  in superAggregateX

- start_value:

  If TRUE, the initial values are added under the year `prev_year`

- digits:

  The result will be rounded to this number of digits

- prev_year:

  Year to store the initialization tau information in

- type:

  currently only "crop"

## Value

A MAgPIE object containing tau values (index)

## Author

Jan Philipp Dietrich, Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
x <- tau(gdx)
} # }
```
