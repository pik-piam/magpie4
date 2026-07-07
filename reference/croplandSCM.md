# croplandSCM

reads cropland area under soil carbon management (SCM) out of a MAgPIE
gdx file (Mha)

## Usage

``` r
croplandSCM(gdx, level = "reg", crop_aggr = TRUE)
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level, "reg", "glo", "regglo", or "cell"

- crop_aggr:

  aggregate over crop types (TRUE) or report by crop (FALSE)

## Value

SCM area as MAgPIE object (mio. ha)

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
x <- croplandSCM(gdx)
} # }
```
