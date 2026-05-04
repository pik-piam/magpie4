# trade

Calculates MAgPIE trade or self-sufficiencies out of a gdx file

## Usage

``` r
trade(
  gdx,
  file = NULL,
  level = "reg",
  products = "k_trade",
  productAggr = FALSE,
  attributes = "dm",
  weight = FALSE,
  relative = FALSE,
  type = "net-exports"
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation ("reg", "glo", "regglo", or name of
  custom mapping)

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr")

- productAggr:

  aggregate over products or not (boolean)

- attributes:

  dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt
  ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm").
  Can also be a vector.

- weight:

  in case relative=T also the weighting for the self sufficiencies is
  provided as it is an intensive parameter

- relative:

  if relative=TRUE, self sufficiencies are reported, so the amount of
  production divided by domestic demand

- type:

  exports-imports ("net-exports"), gross imports ("imports") or gross
  exports ("exports"), in the bilateral case we report a few others
  given balanceflow: ; only valid if relative=FALSE

## Value

trade (production-demand) as MAgPIE object; unit depends on attributes

## Details

Trade definitions are equivalent to FAO CBS categories

## Author

Benjamin Leon Bodirsky, Florian Humpenoeder, Mishko Stevanovic

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- trade(gdx="fulldata.gdx", level="regglo", products="kcr")
  } # }
```
