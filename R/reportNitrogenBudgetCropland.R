#' @title reportNitrogenBudgetCropland
#' @description Reports the Nitrogen Budgets of Croplands for future MAgPIE projections
#'
#' @importFrom magpiesets reportingnames
#' @export
#'
#' @param gdx GDX file
#' @param include_emissions TRUE also divides the N surplus into different emissions
#' @param grid grid provides outputs on grid level of 0.5 degree
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{NitrogenBudget}}
#'
#' @examples
#' \dontrun{
#' x <- reportNitrogenBudgetCropland(gdx)
#' }
#'
#' @section Nitrogen inputs variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Nitrogen\|Cropland Budget\|Inputs | Mt Nr/yr | Total nitrogen inputs to cropland
#' Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|Fertilizer | Mt Nr/yr | Synthetic fertilizer nitrogen applied to cropland
#' Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|Manure | Mt Nr/yr | Manure nitrogen applied to cropland
#' Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|BNF | Mt Nr/yr | Biological nitrogen fixation on cropland
#' Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|Atmospheric deposition | Mt Nr/yr | Atmospheric nitrogen deposition on cropland
#' Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|Seeds | Mt Nr/yr | Nitrogen from seeds
#'
#' @section Nitrogen withdrawals variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Nitrogen\|Cropland Budget\|Withdrawals | Mt Nr/yr | Total nitrogen withdrawals from cropland
#' Resources\|Nitrogen\|Cropland Budget\|Withdrawals\|+\|Harvest | Mt Nr/yr | Nitrogen removed in harvested crops
#' Resources\|Nitrogen\|Cropland Budget\|Withdrawals\|+\|Above-ground residues | Mt Nr/yr | Nitrogen in above-ground crop residues
#' Resources\|Nitrogen\|Cropland Budget\|Withdrawals\|+\|Below-ground residues | Mt Nr/yr | Nitrogen in below-ground crop residues
#'
#' @section Nitrogen balance variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Nitrogen\|Cropland Budget\|Balance | Mt Nr/yr | Total nitrogen balance on cropland
#' Resources\|Nitrogen\|Cropland Budget\|Balance\|+\|Surplus | Mt Nr/yr | Nitrogen surplus on cropland (inputs minus withdrawals)
#' Resources\|Nitrogen\|Cropland Budget\|Balance\|+\|Soil organic matter | Mt Nr/yr | Net nitrogen flow into soil organic matter (negative = release)
#' Resources\|Nitrogen\|Cropland Budget\|Balance\|+\|Balance flow | Mt Nr/yr | Nitrogen balance flow (calibration term)
#' @md

#'
reportNitrogenBudgetCropland <- function(gdx, include_emissions = FALSE, grid = FALSE, level = "regglo") {

  if (grid == FALSE) {
    budget <- NitrogenBudget(gdx, level = level, include_emissions = include_emissions)
    budget[, , "som"] <- -budget[, , "som"]


    all <- getNames(budget)
    withdrawaltypes <- c("harvest", "ag", "bg")
    balancetypes <- c("surplus", "som", "balanceflow")
    if (include_emissions) {
      emissiontypes <- c("n2o_n", "nh3_n", "no2_n", "no3_n")
    } else {
      emissiontypes <- NULL
    }
    inputtypes <- setdiff(all, c(withdrawaltypes, balancetypes, emissiontypes))

    tmp <- budget[, , inputtypes]
    getNames(tmp) <- paste0("Resources|Nitrogen|Cropland Budget|Inputs|+|", reportingnames(getNames(tmp)))
    inputs <- mbind(
      setNames(dimSums(tmp, dim = 3), "Resources|Nitrogen|Cropland Budget|Inputs"),
      tmp
    )

    tmp <- budget[, , withdrawaltypes]
    getNames(tmp) <- paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|+|", reportingnames(getNames(tmp)))
    withdrawals <- mbind(
      setNames(dimSums(tmp, dim = 3), "Resources|Nitrogen|Cropland Budget|Withdrawals"),
      tmp
    )

    tmp <- budget[, , balancetypes]
    getNames(tmp) <- paste0("Resources|Nitrogen|Cropland Budget|Balance|+|", reportingnames(getNames(tmp)))
    balance <- mbind(
      setNames(dimSums(tmp, dim = 3), "Resources|Nitrogen|Cropland Budget|Balance"),
      tmp
    )

    if (include_emissions) {
      tmp <- budget[, , emissiontypes]
      getNames(tmp) <- paste0("Resources|Nitrogen|Cropland Budget|Balance|Nutrient Surplus|", reportingnames(getNames(tmp)))
      emissions <- tmp
    } else {
      emissions <- NULL
    }

    out <- mbind(
      inputs,
      withdrawals,
      balance,
      emissions
    )

    getNames(out) <- paste0(getNames(out), " (Mt Nr/yr)")

  } else {

    out <- NitrogenBudget(gdx, level = "grid", include_emissions = include_emissions)
    out[, , "som"] <- -out[, , "som"]
    getNames(out) <- reportingnames(getNames(out))
    descr <- paste0("Total land area in its primary land cover categories.",
      " Other includes non-forest natural vegetation like savannas. Soil organic matter quantifies the nitrogen flow into soil stocks; negative numbers is released Nr from soil depletion.")
    out <- metadata_comments(x = out, unit = "Mt Nr/yr", description = descr, comment = "", note = "")
  }

  return(out)
}
