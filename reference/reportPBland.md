# reportPBland

reports land planetary boundary: forest area

## Usage

``` r
reportPBland(gdx, level = "regglo", foresttype = "all")
```

## Arguments

- gdx:

  GDX file

- level:

  level of aggregation (regglo: regions and global)

- foresttype:

  managed forest types that are included in the calculation of the
  forest area (all: all managed forests, noTimber: timber plantations
  are not counted)

## Value

MAgPIE object

## Land planetary boundary variables

|                                        |      |                                                 |
|----------------------------------------|------|-------------------------------------------------|
| Name                                   | Unit | Meta                                            |
| Planetary Boundary\|Land\|Forest cover | Mha  | Total forest area (natural and managed forests) |

## Author

Felicitas Beier, Patrick von Jeetze

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportPBland(gdx)
  } # }

```
