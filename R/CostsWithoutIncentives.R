#' @title CostsWithoutIncentives
#' @description calculates agricultural costs without taxes and incentives (i.e. GHG taxes and BII incentives)
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, reg, glo or regglo
#' @author David M Chen

#' @examples
#' \dontrun{
#' x <- CostsWithoutIncentives(gdx)
#' }
#'
CostsWithoutIncentives <- function(gdx, file = NULL, level = "regglo") {
  # list of current and past incentives for b-wards compatibility
  incentives <- c("GHG Emissions", "Reward for Afforestation",
                  "Biodiversity", "Biodiversity value loss",
                  "Reward for producing bioenergy",
                  "Peatland GHG emisssions", "Peatland",
                  "Punishment urban deviation",
                  "Punishment cost for additionally transported monogastric livst_egg"
  ) # nolint
  # use costs investment type (costs are one-off at that time step, not amortized)
  totCosts <- costs(gdx = gdx, level = level, type = "investment", sum = FALSE)
  # take out those incentives that are present
  totCosts <- totCosts[, , -which(getNames(totCosts) %in% incentives)]
  # remove wage rent from input factor costs
  totCosts[, , "Input Factors"] <- totCosts[, , "Input Factors"] - wageRent(gdx = gdx, level = level)
  # remove tax Revenue from input factor costs
  totCosts[, , "Penalty or tax for violating crop rotations"] <- totCosts[, ,
                                                                 "Penalty or tax for violating crop rotations"] -
                                                                  taxRevenueRotations(gdx = gdx, level = level)

  # removing penalty terms which are not explicitly module interfaces
  # when possible using a dummy cost for manna from heaven.
  dummy_cost=readGDX(gdx,"f15_prices_initial")
  # penalty of forestry targets cannot be met

  ov32_land_missing <-  readGDX(gdx=gdx, "ov32_land_missing", select = list(type = "level"), react = "silent")
  s32_free_land_cost <-  readGDX(gdx=gdx, "s32_free_land_cost", react = "silent")
  if(is.null(ov32_land_missing)|is.null(s32_free_land_cost)){
    cat("ov32_land_missing or s32_free_land_cost do not exist in this version of the model")
  } else {
    penalty_forestry <- gdxAggregate(gdx=gdx, x=ov32_land_missing*s32_free_land_cost, weight=NULL, to=level)
    totCosts[, , "Forestry"] <-  totCosts[, , "Forestry"] - dimSums(penalty_forestry,dim=3.1)
  }

  ov32_land_missing_ndc <-  readGDX(gdx=gdx, "ov32_land_missing_ndc", select = list(type = "level"), react = "silent")
  s32_free_land_cost <-  readGDX(gdx=gdx, "s32_free_land_cost", react = "silent")
  if(is.null(ov32_land_missing_ndc)|is.null(s32_free_land_cost)){
    cat("ov32_land_missing_ndc or s32_free_land_cost do not exist in this version of the model")
  } else {
    penalty_ndc <- gdxAggregate(gdx=gdx, x=ov32_land_missing_ndc*s32_free_land_cost, weight=NULL, to=level)
    totCosts[, , "Forestry"] <-  totCosts[, , "Forestry"] - dimSums(penalty_ndc,dim=3.1)
  }

  # penalty of timber targets cannot be met
  ov73_prod_heaven_timber = readGDX(gdx=gdx, "ov73_prod_heaven_timber", select = list(type = "level"), react = "silent")
  s73_free_prod_cost = readGDX(gdx=gdx, "s73_free_prod_cost", react = "silent")
  if(is.null(ov73_prod_heaven_timber)|is.null(s73_free_prod_cost)){
    cat("ov73_prod_heaven_timber or s73_free_prod_cost do not exist in this version of the model")
  } else {
    penalty_timber <- gdxAggregate(gdx=gdx, x=ov73_prod_heaven_timber*(s73_free_prod_cost-dummy_cost[,,getNames(ov73_prod_heaven_timber)]), weight=NULL, to=level)
    totCosts[, , "Timber production"] <-  totCosts[, , "Timber production"] - dimSums(penalty_timber,dim=3.1)

  }

  # penalty of timber targets cannot be met
  ov21_manna_from_heaven = readGDX(gdx=gdx, "ov21_manna_from_heaven", select = list(type = "level"), react = "silent")
  if(is.null(ov21_manna_from_heaven)){
    message("ov21_manna_from_heaven does not exist in this version of the model")
  } else {
    penalty_trade <- gdxAggregate(gdx=gdx, x=ov21_manna_from_heaven*(10^6-dummy_cost[,,getNames(ov21_manna_from_heaven)]), weight=NULL, to=level)
    totCosts[, , "Trade"] <-  totCosts[, , "Trade"] - dimSums(penalty_trade,dim=3.1)
  }

  # peatland costs without slack are in v58_peatland_cost in realization "on"
  peatlandCosts <- readGDX(gdx, "ov58_peatland_cost", select = list(type = "level"), react = "silent")
  # v58_peatland_cost does not exist in realization "v2" because there is no slack variable
  if(is.null(peatlandCosts)) peatlandCosts <- readGDX(gdx, "ov_peatland_cost", select = list(type = "level"), react = "silent")
  totCosts <- add_columns(totCosts, addnm = "Peatland", dim = 3.1, fill = 0)

  if (!is.null(peatlandCosts)) {
    peatlandCosts <- gdxAggregate(gdx=gdx, x=peatlandCosts, weight=NULL, to=level)
    totCosts[, , "Peatland"] <- peatlandCosts
  }

  ov58_balance <- readGDX(gdx, "ov58_balance", select = list(type = "level"), react = "silent")
  if (!is.null(ov58_balance)) {
    ov58_balance <- gdxAggregate(gdx=gdx, x=ov58_balance, weight=NULL, to=level)
    totCosts[, , "Peatland"] <- totCosts[, , "Peatland"] - ov58_balance
  }

  totCosts <- dimSums(totCosts, dim = 3)

  return(totCosts)

}
