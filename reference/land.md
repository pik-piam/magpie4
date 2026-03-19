# land

reads land out of a MAgPIE gdx file

## Usage

``` r
land(
  gdx,
  file = NULL,
  level = "reg",
  types = NULL,
  subcategories = NULL,
  sum = FALSE
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
  in gdxAggregate

- types:

  NULL or a vector of strings. If NULL, all land types are used. Options
  are "crop", "past", "forestry", "primforest","secdforest, "urban",
  "other", "primother" and "secdother"

- subcategories:

  NULL or vector of strings. If NULL, no subcategories are returned.
  Meaningful options are "crop, "forestry" and "other"

- sum:

  determines whether output should be land-type-specific (FALSE) or
  aggregated over all types (TRUE).

## Value

land as MAgPIE object (Mha)

## See also

[`reportLandUse`](reportLandUse.md)

## Author

Jan Philipp Dietrich, Florian Humpenoeder, Benjamin Leon Bodirsky,
Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
x <- land(gdx)
} # }
```
