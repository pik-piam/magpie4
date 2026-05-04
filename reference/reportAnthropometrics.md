# reportAnthropometrics

reports Underweight, Normalweight, Overweight and Obesity as well as
body height for males and females

## Usage

``` r
reportAnthropometrics(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation: "reg", "glo", "regglo", "iso"

## Value

Magpie object

## Anthropometrics variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Nutrition\|Anthropometrics\|People underweight | million people | Population with underweight BMI |
| Nutrition\|Anthropometrics\|People normalweight | million people | Population with normal BMI |
| Nutrition\|Anthropometrics\|People overweight | million people | Population with overweight BMI |
| Nutrition\|Anthropometrics\|People obese | million people | Population with obesity |
| Nutrition\|Anthropometrics\|Body height of female adults | cm/capita | Average body height of adult females |
| Nutrition\|Anthropometrics\|Body height of male adults | cm/capita | Average body height of adult males |

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportBodyweight(gdx)
} # }
```
