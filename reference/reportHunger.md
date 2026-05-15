# reportHunger

Calculates the share of people living in hunger.

## Usage

``` r
reportHunger(gdx)
```

## Arguments

- gdx:

  GDX file

## Value

magpie object with hunger (mio people) or hunger share

## Hunger variables

|                                                                 |               |                                    |
|-----------------------------------------------------------------|---------------|------------------------------------|
| Name                                                            | Unit          | Meta                               |
| Food Supply\|Calorie Supply\|Undernourished                     | Mio People    | Number of undernourished people    |
| Food Supply\|Calorie Supply\|Share of population undernourished | People/People | Share of population undernourished |

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportHunger(gdx)
  } # }
```
