# reportTimber

reports MAgPIE demand for timber.

## Usage

``` r
reportTimber(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Timber demand

## Timber demand variables

|                                                                   |        |                                        |
|-------------------------------------------------------------------|--------|----------------------------------------|
| Name                                                              | Unit   | Meta                                   |
| Timber\|Volumetric\|Demand\|+\|Roundwood                          | Mm3/yr | Total roundwood demand                 |
| Timber\|Volumetric\|Demand\|Roundwood\|+\|Industrial roundwood    | Mm3/yr | Demand for industrial roundwood        |
| Timber\|Volumetric\|Demand\|Roundwood\|+\|Wood fuel               | Mm3/yr | Demand for wood fuel                   |
| Timber\|Volumetric\|Demand\|Roundwood\|+\|Timber for construction | Mm3/yr | Demand for timber used in construction |

## Timber production variables

|                                                                       |        |                                       |
|-----------------------------------------------------------------------|--------|---------------------------------------|
| Name                                                                  | Unit   | Meta                                  |
| Timber\|Volumetric\|Production\|+\|Roundwood                          | Mm3/yr | Total roundwood production            |
| Timber\|Volumetric\|Production\|Roundwood\|+\|Industrial roundwood    | Mm3/yr | Production of industrial roundwood    |
| Timber\|Volumetric\|Production\|Roundwood\|+\|Wood fuel               | Mm3/yr | Production of wood fuel               |
| Timber\|Volumetric\|Production\|Roundwood\|+\|Timber for construction | Mm3/yr | Production of timber for construction |

## Timber trade variables

|                                                                      |        |                                          |
|----------------------------------------------------------------------|--------|------------------------------------------|
| Name                                                                 | Unit   | Meta                                     |
| Timber\|Volumetric\|Net-Trade\|+\|Roundwood                          | Mm3/yr | Net export of roundwood                  |
| Timber\|Volumetric\|Net-Trade\|Roundwood\|+\|Industrial roundwood    | Mm3/yr | Net export of industrial roundwood       |
| Timber\|Volumetric\|Net-Trade\|Roundwood\|+\|Wood fuel               | Mm3/yr | Net export of wood fuel                  |
| Timber\|Volumetric\|Net-Trade\|Roundwood\|+\|Timber for construction | Mm3/yr | Net export of timber for construction    |
| Timber\|Volumetric\|Exports\|+\|Roundwood                            | Mm3/yr | Gross exports of roundwood               |
| Timber\|Volumetric\|Exports\|Roundwood\|+\|Industrial roundwood      | Mm3/yr | Gross exports of industrial roundwood    |
| Timber\|Volumetric\|Exports\|Roundwood\|+\|Wood fuel                 | Mm3/yr | Gross exports of wood fuel               |
| Timber\|Volumetric\|Exports\|Roundwood\|+\|Timber for construction   | Mm3/yr | Gross exports of timber for construction |
| Timber\|Volumetric\|Imports\|+\|Roundwood                            | Mm3/yr | Gross imports of roundwood               |
| Timber\|Volumetric\|Imports\|Roundwood\|+\|Industrial roundwood      | Mm3/yr | Gross imports of industrial roundwood    |
| Timber\|Volumetric\|Imports\|Roundwood\|+\|Wood fuel                 | Mm3/yr | Gross imports of wood fuel               |
| Timber\|Volumetric\|Imports\|Roundwood\|+\|Timber for construction   | Mm3/yr | Gross imports of timber for construction |

## Author

Abhijeet Mishra

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportTimber(gdx)
  } # }

```
