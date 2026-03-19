# landConservation

reads land conservation information out of a MAgPIE gdx file. Land
restoration `'restore'` is reported in Mha/yr by default but can be also
reported both over the time step length and cumulatively.

## Usage

``` r
landConservation(
  gdx,
  file = NULL,
  level = "cell",
  cumuRestor = FALSE,
  baseyear = 1995,
  annualRestor = FALSE,
  sum = FALSE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "grid", "iso, "reg" (regional),
  "glo" (global), "regglo" (regional and global) or any secdforest
  aggregation level defined in superAggregate

- cumuRestor:

  Logical; Whether function should report cumulative land restoration.

- baseyear:

  Base year used for cumulative land restoration reporting (default =
  1995)

- annualRestor:

  Logical; Whether function should report annual land restoration.

- sum:

  sum over land pools (default = FALSE)

## Value

protected area in Mha

## Details

protected areas in primforest, secdforest and other land

## Author

Florian Humpenoeder, Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
x <- landConservation(gdx)
} # }
```
