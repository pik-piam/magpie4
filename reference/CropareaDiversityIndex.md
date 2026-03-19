# CropareaDiversityIndex

calculates an index that measures the croparea diversity

## Usage

``` r
CropareaDiversityIndex(
  gdx,
  index = "shannon",
  level = "reg",
  measurelevel = "cell",
  groupdiv = "agg1"
)
```

## Arguments

- gdx:

  GDX file

- index:

  can be "shannon", "gini" or "invsimpson" for different types of
  diversitiy indices

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- measurelevel:

  level at which diversity is measured. "cell" means diversity

- groupdiv:

  should crop groups be split up into several individual items or not?
  Choose either FALSE or different (dis)aggregation methods "agg1",
  "agg2"

## Value

MAgPIE object (unit depends on attributes)

## See also

`CropareaDiversityIndex`

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
x <- CropareaDiversityIndex(gdx)
} # }
```
