# cropland

reads cropland out of a MAgPIE gdx file. Cropland includes croparea plus
fallow plus cropland with tree cover

## Usage

``` r
cropland(gdx, level = "reg", types = NULL, sum = FALSE)
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in gdxAggregate

- types:

  NULL or a vector of strings. If NULL, all land types are used. Options
  are "crop_area", "crop_fallow", "crop_treecover"

- sum:

  determines whether output should be land-type-specific (FALSE) or
  aggregated over all types (TRUE).

## Value

land as MAgPIE object (Mha)

## See also

[`reportLandUse`](reportLandUse.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
x <- cropland(gdx)
} # }
```
