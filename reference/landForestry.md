# landForestry

reads and compiles forestry land subcategories from a MAgPIE gdx file

## Usage

``` r
landForestry(gdx, file = NULL, level = "reg")
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

## Value

land as MAgPIE object (Mha)

## See also

[`reportLandUse`](reportLandUse.md)

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- land(gdx)
  } # }
```
