# AgriResearchIntensity

calculates Agricultural Research Intensity (Investment in AgR&D/Total
GDP) from a MAgPIE gdx file

## Usage

``` r
AgriResearchIntensity(gdx, file = NULL, level = "reg")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  aggregation level, reg, glo or regglo, cell or grid

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- AgriResearchIntensity(gdx)
} # }
```
