#' @title FoodDemandModuleConsumerPrices
#' @description Calculates food prices that enter demand module
#'
#' @export
#'
#' @param gdx GDX file
#' @param level reg or iso
#' @param valueAdded whether to add the value-added
#' marketing margin to the total expenditures
#'
#'
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- FoodDemandModuleConsumerPrices(gdx)
#'   }
#'

FoodDemandModuleConsumerPrices <- memoise(function(gdx, level = "iso", valueAdded = FALSE) {
  #MAgPIE versions previous to version 4.10 had a bug where lastIter() returned only 0
  #only the 1st interation contains values for food prices
  # here we use p15_tax_recycling as a proxy for version checking as it was introduced
  # in v4.10
  versionCheck <- suppressWarnings(readGDX(gdx, "p15_tax_recycling"))

  if (is.null(versionCheck)) {
    price <- collapseNames(readGDX(gdx, "p15_prices_kcal")[, , "iter1"])
  } else {
    price <- lastIter(gdx, "p15_prices_kcal")
  }

  if ((!identical(valueAdded, FALSE))) {
    #check if margin exists otherwise read in coef files to make backwards compatible
    margin <- suppressWarnings((readGDX(gdx, "p15_marketing_margin_fah_kcal")))
    if (is.null(margin)) {
      # make backwards compatible with input values in the mapping folder for now
      markupCoef <- read.csv(system.file("extdata", "Markup_coef.csv", package = "magpie4"))
      colnames(markupCoef) <- NULL
      colnames(markupCoef) <- markupCoef[1, ]
      markupCoef <- markupCoef[2:nrow(markupCoef), c(1:5)]
      markupCoef <- tidyr::pivot_longer(markupCoef, cols = c("a", "b", "c"), names_to = "coef", values_to = "value")
      markupCoef$value <- as.double(markupCoef$value)
      markupCoef <- as.magpie(markupCoef, spatial = "GLO", temporal = "y2010", tidy = TRUE)
      gdp <- readGDX(gdx, "im_gdp_pc_mer_iso")
      attr <- readGDX(gdx, "fm_attributes")
      nutrAttr <- readGDX(gdx, "fm_nutrition_attributes")
      kcalPcIso <- readGDX(gdx, "p15_kcal_pc_iso")
    }

    if (valueAdded == "valueAddedFAH") {
      margin <- suppressWarnings((readGDX(gdx, "p15_marketing_margin_fah_kcal")))

      if (is.null(margin)) {
        margin <- (markupCoef[, , "fah"][, , "a"] * (markupCoef[, , "fah"][, , "b"]^log(gdp)) + markupCoef[, , "fah"][, , "c"]) * attr[, , "wm"][, , getItems(markupCoef, dim = 3.1)]
        margin <- collapseNames(margin / (nutrAttr[, getYears(margin), getItems(markupCoef, dim = 3.1)][, , "kcal"] * 10^6))
      }
    } else if (valueAdded == "valueAddedFAFH") {
      margin <- suppressWarnings((readGDX(gdx, "p15_marketing_margin_fafh_kcal")))

      if (is.null(margin)) {
        margin <- (markupCoef[, , "fafh"][, , "a"] * markupCoef[, , "fafh"][, , "b"]^log(gdp) + markupCoef[, , "fafh"][, , "c"]) * attr[, , "wm"][, , getItems(markupCoef, dim = 3.1)]
        margin <- collapseNames(margin / (nutrAttr[, getYears(margin), getItems(markupCoef, dim = 3.1)][, , "kcal"] * 10^6))
      }
    } else {
      warning("Food value added is either valueAddedFAH or valueAddedFAFH")
    }
    #convert margin to PPP
    margin <- convertGDP(margin, unit_in = "constant 2017 US$MER", unit_out = "constant 2017 Int$PPP", replace_NAs = "with_USA")

    price <- price + margin[, getYears(price), ]
  }

  if (level == "reg") {
    out = gdxAggregate(gdx, price, weight = price * 0 + 1, to = "reg", absolute = FALSE)
  } else if (level == "iso") {
    out = price
  } else {
    (stop("undefined level"))
  }

  return(out)
}
# the following line makes sure that a changing timestamp of the gdx file and
# a working directory change leads to new caching, which is important if the
# function is called with relative path args.
, hash = function(x) hash(list(x, getwd(), lastModified(x$gdx))))
