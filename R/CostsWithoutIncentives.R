#' @title CostsWithoutIncentives
#' @description calculates agricultural costs without taxes, incentives and technical penalty costs (i.e. GHG taxes and BII incentives)
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, reg, glo or regglo
#' @return A MAgPIE object containing the costs without taxes, incentives and technical penalty costs [million US$17]
#' @author David M Chen
#' @examples
#' \dontrun{
#'   x <- CostsWithoutIncentives(gdx)
#' }
#' @export
CostsWithoutIncentives <- function(gdx, file = NULL, level = "regglo") {
  # list of current and past incentives for b-wards compatibility
  incentives <- c(
    "GHG Emissions", "Reward for Afforestation",
    "Biodiversity", "Biodiversity value loss",
    "Reward for producing bioenergy",
    "Peatland GHG emisssions", "Peatland",
    "Punishment urban deviation",
    "Punishment cost for additionally transported monogastric livst_egg"
  ) # nolint

  # NULL for variables which do not exist in all model versions and realizations
  readLevel <- function(name) {
    return(readGDX(gdx, name, select = list(type = "level"), react = "silent"))
  }

  aggregateToLevel <- function(x) {
    return(gdxAggregate(gdx = gdx, x = x, weight = NULL, to = level))
  }

  # use costs investment type (costs are one-off at that time step, not amortized)
  totCosts <- costs(gdx = gdx, level = level, type = "investment", sum = FALSE)
  # take out those incentives that are present
  totCosts <- totCosts[, , -which(getNames(totCosts) %in% incentives)]
  # remove wage rent from input factor costs
  totCosts[, , "Input Factors"] <- totCosts[, , "Input Factors"] - wageRent(gdx = gdx, level = level)
  # remove tax Revenue from input factor costs
  totCosts[, , "Penalty or tax for violating crop rotations"] <-
    totCosts[, , "Penalty or tax for violating crop rotations"] - dimSums(taxRevenueRotations(gdx = gdx, level = level), dim = 3)

  # removing penalty terms which are not explicitly module interfaces
  # when possible using a dummy cost for manna from heaven.
  dummyCost <- readGDX(gdx, "f15_prices_initial")

  # penalty if forestry targets cannot be met
  freeLandCost <- readGDX(gdx, "s32_free_land_cost", react = "silent")
  for (landMissingName in c("ov32_land_missing", "ov32_land_missing_ndc")) {
    landMissing <- readLevel(landMissingName)
    if (is.null(landMissing) || is.null(freeLandCost)) {
      message(landMissingName, " or s32_free_land_cost do not exist in this version of the model")
    } else {
      totCosts[, , "Forestry"] <- totCosts[, , "Forestry"] -
        dimSums(aggregateToLevel(landMissing * freeLandCost), dim = 3.1)
    }
  }

  # penalty if timber targets cannot be met
  prodHeavenTimber <- readLevel("ov73_prod_heaven_timber")
  freeProdCost <- readGDX(gdx, "s73_free_prod_cost", react = "silent")
  if (is.null(prodHeavenTimber) || is.null(freeProdCost)) {
    message("ov73_prod_heaven_timber or s73_free_prod_cost do not exist in this version of the model")
  } else {
    penaltyTimber <- prodHeavenTimber * (freeProdCost - dummyCost[, , getNames(prodHeavenTimber)])
    totCosts[, , "Timber production"] <- totCosts[, , "Timber production"] -
      dimSums(aggregateToLevel(penaltyTimber), dim = 3.1)
  }

  # penalty if trade balances cannot be met
  mannaFromHeaven <- readLevel("ov21_manna_from_heaven")
  if (is.null(mannaFromHeaven)) {
    message("ov21_manna_from_heaven does not exist in this version of the model")
  } else {
    penaltyTrade <- mannaFromHeaven * (10^6 - dummyCost[, , getNames(mannaFromHeaven)])
    totCosts[, , "Trade"] <- totCosts[, , "Trade"] - dimSums(aggregateToLevel(penaltyTrade), dim = 3.1)
  }

  # peatland costs without slack are in v58_peatland_cost in realization "on"
  peatlandCosts <- readLevel("ov58_peatland_cost")
  # v58_peatland_cost does not exist in realization "v2"
  if (is.null(peatlandCosts)) peatlandCosts <- readLevel("ov_peatland_cost")
  totCosts <- add_columns(totCosts, addnm = "Peatland", dim = 3.1, fill = 0)

  if (!is.null(peatlandCosts)) {
    totCosts[, , "Peatland"] <- aggregateToLevel(peatlandCosts)
  }

  # s58_balance_penalty is read inside the guard, it does not exist in versions without the balances
  for (balanceName in c("ov58_balance", "ov58_balance2")) {
    balance <- readLevel(balanceName)
    if (!is.null(balance)) {
      totCosts[, , "Peatland"] <- totCosts[, , "Peatland"] -
        dimSums(aggregateToLevel(balance), dim = 3) * readGDX(gdx, "s58_balance_penalty")
    }
  }

  totCosts <- dimSums(totCosts, dim = 3)

  return(totCosts)

}
