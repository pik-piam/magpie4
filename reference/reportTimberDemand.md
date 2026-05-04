# reportTimberDemand

reports MAgPIE demand for timber in tDM (complementing
[`reportTimber`](reportTimber.md) which reports in Mm3).

## Usage

``` r
reportTimberDemand(gdx)
```

## Arguments

- gdx:

  GDX file

## Value

Timber demand

## Timber demand variables

|                                     |         |                        |
|-------------------------------------|---------|------------------------|
| Name                                | Unit    | Meta                   |
| Timber demand\|Roundwood            | mio tDM | Total roundwood demand |
| Timber demand\|Industrial roundwood | mio tDM | Industrial wood demand |
| Timber demand\|Wood fuel            | mio tDM | Wood fuel demand       |

## Author

Abhijeet Mishra

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportTimberDemand(gdx)
  } # }

```
