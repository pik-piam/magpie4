# NetForestChange

Calculates net and gross forest area change based on a MAgPIE gdx file

## Usage

``` r
NetForestChange(gdx, file = NULL, level = "cell", lowpass = NULL)
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

- lowpass:

  number of lowpass filter iterations (default = NULL)

## Value

Net Forest Change as MAgPIE object (Mha per year)

## Author

Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- NetForestChange(gdx)
} # }
```
