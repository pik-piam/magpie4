#' @title       expenditureIndexFood
#' @description calculates food expenditure index (baseyear = 100)
#'              corrected for ghg emission costs based on a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx        GDX file
#' @param file       File the output should be written to using write.magpie
#' @param level      Level of regional aggregation; "reg" (regional),
#'                   "glo" (global), "regglo" (regional and global)
#'                   or any other aggregation level defined in mapping
#' @param products   Selection of products (either by naming products,
#'                   e.g. "tece", or naming a set, e.g."kcr")
#' @param baseyear   Baseyear of the price index
#' @param basketyear Year of reference food basket
#'                   (should be in the past for comparison of different runs to
#'                   have identical and comparable food basket)
#' @param round      Rounded result (TRUE or FALSE)
#' @param ghgtax     Correction of food price expenditure for ghg emission costs
#'                   (TRUE or FALSE)
#' @param valueAdded whether to include value added
#'
#' @return A MAgPIE object containing food price expenditure index, in 2017Int$PPP
#'
#' @author Felicitas Beier, David Chen
#'
#' @examples
#' \dontrun{
#' x <- expenditureIndexFood(gdx)
#' }
#'
#' @importFrom magclass setYears getRegions dimSums getNames new.magpie
#' @importFrom madrat toolAggregate

expenditureIndexFood <- function(gdx, file = NULL, level = "reg",
                                 products = "kfo",
                                 basketyear = "y2010", baseyear = "y2010",
                                 round = TRUE, ghgtax = TRUE,
                                 valueAdded = FALSE) {

  # Read in representative food basket: countries' kcal consumption from the food demand model (kcal / day)
  products   <- readGDX(gdx, products)
  foodbasket <- setYears(Kcal(gdx = gdx, level = "iso", calibrated = TRUE,
                              after_shock = TRUE, products = "kfo", attributes = "kcal",
                              product_aggr = FALSE, per_capita = FALSE)[, basketyear, ], NULL)

  # Agricultural Prices: Prices from MAgPIE after optimization (mio. USD17PPP per kcal)
  # or value added prices additive to the agricultural prices
  if (valueAdded) {
    priceFAH <- FoodDemandModuleConsumerPrices(gdx, valueAdded = "valueAddedFAH")
    priceFAFH <- FoodDemandModuleConsumerPrices(gdx, valueAdded = "valueAddedFAFH")
    fafhShr <- suppressWarnings(readGDX(gdx, "p15_shr_fafh"))

    #make backwards compatibale
    if (is.null(fafhShr)) {
      fafhCoef <- read.csv(system.file("extdata", "Fafh_coef.csv", package = "magpie4"))
      colnames(fafhCoef) <- NULL
      fafhCoef <- as.magpie(fafhCoef[, c(1, 2)], temporal = "y2010", spatial = NULL, tidy = TRUE)
      gdp <- readGDX(gdx, "im_gdp_pc_mer_iso")

      fafhShr = fafhCoef[, , "a_fafh"] + fafhCoef[, , "b_fafh"] * gdp[, getYears(priceFAH), ]
      fafhShr[fafhShr > 1] <- 1
      fafhShr[fafhShr < 0] <- 0
    }

    food_expenditure <- (foodbasket * priceFAH * (1 - fafhShr)) + (foodbasket * priceFAFH * fafhShr)
    food_expenditure <- food_expenditure * 365
  } else {
    price <- FoodDemandModuleConsumerPrices(gdx)
    # Food expenditure per country per product
    food_expenditure <- foodbasket * price * 365
  }

  # Aggregation to regions
  mapping          <- readGDX(gdx, "i_to_iso")
  food_expenditure <- toolAggregate(food_expenditure, rel = mapping,
                                    from = "iso", to = "i")

  # Total food expenditure
  food_expenditure_total <- dimSums(food_expenditure, dim = 3)
  # Share of single food products in total food expenditure
  foodproduct_shr        <- food_expenditure / food_expenditure_total

  if (ghgtax) {
    # Emission costs
    .tmp_cost <- function(gdx, name, label) {
      cost <- readGDX(gdx, name, format = "first_found",
                      select = list(type = "level"), react = "quiet")
      if (is.null(cost)) return(NULL)
      cost <- dimSums(cost, dim = 3)
      cost <- superAggregateX(cost, aggr_type = "sum", level = "reg")
      dimnames(cost)[[3]] <- label

      return(cost)
    }

    # Costs for emission rights for pollutants and greenhouse gases (mio. USD05MER per yr)
    ghg_tax     <- .tmp_cost(gdx, "ov_emission_costs", "GHG Emissions")
    ghg_tax     <- collapseNames(foodproduct_shr * ghg_tax)

    # Corrected food expenditure
    tmp           <- dimSums(food_expenditure[, , products] - ghg_tax[, , products], dim = 3)
    getNames(tmp) <- "Food Expenditure Index corrected for ghg costs"
  } else {
    # Food expenditure
    tmp           <- dimSums(food_expenditure[, , products], dim = 3)
    getNames(tmp) <- "Food Expenditure Index"
  }

  # Reported level
  if (level == "reg") {
    food_expenditure_corrected <- tmp
  } else if (level %in% c("glo", "regglo") || isCustomAggregation(level)) {
    food_expenditure_corrected <- gdxAggregate(gdx, tmp, to = level)
  } else if (level != "iso") {
    stop("iso-level reporting of Food Expenditure Index not supported")
  }

  # Food Expenditure Index (corrected)
  foodexpenditureindex <- food_expenditure_corrected /
    setYears(food_expenditure_corrected[, baseyear, ], NULL)

  # Food Expenditure Index with baseyear=100
  foodexpenditureindex <- foodexpenditureindex * 100
  if (round) {
    foodexpenditureindex <- round(foodexpenditureindex, 2)
  }

  out(foodexpenditureindex, file)
}
