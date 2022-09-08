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
#'
#' @return A MAgPIE object containing food price expenditure index
#'
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' x <- expenditureIndexFood(gdx)
#' }
#'
#' @importFrom magclass setYears getRegions dimSums getNames new.magpie
#' @importFrom madrat toolAggregate
#' @importFrom luscale superAggregate

expenditureIndexFood <- function(gdx, file = NULL, level = "reg",
                                 products = "kfo",
                                 basketyear = "y2010", baseyear = "y2010",
                                 round = TRUE, ghgtax = TRUE) {

  # Read in representative food basket: countries' cal consumption from the food demand model (cal / day)
  products   <- readGDX(gdx, products)
  foodbasket <- setYears(Kcal(gdx = gdx, level = "iso", calibrated = TRUE,
                              after_shock = TRUE, products = "kfo", attributes = "kcal",
                              product_aggr = FALSE, per_capita = FALSE)[, basketyear, ], NULL)

  # Agricultural Prices: Prices from MAgPIE after optimization (mio. USD05PPP per kcal)
  price      <- FoodDemandModuleConsumerPrices(gdx)

  # Food expenditure per country per product
  food_expenditure <- (foodbasket * price * 365)
  # Aggregation to image-10 regions
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
      cost <- superAggregate(cost, aggr_type = "sum", level = "reg")
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

  } else if (level == "glo") {

    food_expenditure_corrected <- dimSums(tmp, dim = 1)

  } else if (level == "regglo") {

    food_expenditure_corrected                      <- new.magpie(cells_and_regions =  c(getRegions(tmp), "GLO"),
                                                                  years = getYears(tmp),
                                                                  names = getNames(tmp))
    food_expenditure_corrected[getRegions(tmp), , ] <- tmp[getRegions(tmp), , ]
    food_expenditure_corrected["GLO", , ]           <- dimSums(tmp, dim = 1)

  } else if (level != "iso") {
    stop("iso-level reporting of Food Expenditure Index not supported")
  }

  # Food Expenditure Index (corrected)
  foodexpenditureindex <- food_expenditure_corrected /
                          setYears(food_expenditure_corrected[, baseyear, ],
                                   NULL)

  # Food Expenditure Index with baseyear=100
  foodexpenditureindex            <- foodexpenditureindex * 100
  if (round) foodexpenditureindex <- round(foodexpenditureindex)

  out(foodexpenditureindex, file)
}
