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
  #list of current and past incentives for b-wards compatibility
  incentives <- c("GHG Emissions", "Reward for Afforestation",
                  "Biodiversity", "Biodiversity value loss",
                  "Reward for producing bioenergy",
                  "Peatland GHG emisssions", "Peatland",
                  "Punishment urban deviation",
                  "Punishment cost for additionally transported monogastric livst_egg"
                  ) #nolint

  totCosts <- costsOptimization(gdx = gdx, level = level, type = "investment", sum = FALSE ) #use costsOptimization investment type (costs are one-off at that time step, not amortized)
  totCosts <- totCosts[,,-which(getNames(totCosts) %in% incentives | getNames(totCosts) %in% c("Forestry","Timber production"))] #take out those incentives that are present
  totCosts[, , "Input Factors"] <- totCosts[, , "Input Factors"] - wageRent(gdx = gdx, level = level) #remove wage rent from input factor costs
  totCosts[, , "Penalty or tax for violating crop rotations"] <- totCosts[, , "Penalty or tax for violating crop rotations"] - taxRevenueRotations(gdx = gdx, level = level) #remove tax Revenue from input factor costs


  #peatland costs without slack are in v58_peatland_cost
  peatland_costs <- readGDX(gdx, "ov58_peatland_cost",select = list(type="level"), react = "silent")
  totCosts <- add_columns(totCosts, addnm = "Peatland", dim = 3.1, fill = 0)

  if (!is.null(peatland_costs)) {
    if(level=="reg") {
      totCosts[,,"Peatland"] <- dimSums(peatland_costs,dim=1.2)
    } else if (level == "glo") {
      peatland_costs <- dimSums(peatland_costs, dim = 1)
      getItems(peatland_costs, dim = 1) <- "GLO"
      totCosts[,,"Peatland"] <- dimSums(peatland_costs,dim=1)
    } else if (level=="regglo") {
      peatland_costsGLO <- dimSums(peatland_costs, dim = 1)
      getItems(peatland_costsGLO, dim = 1) <- "GLO"
      totCosts[,,"Peatland"] <- mbind(dimSums(peatland_costs,dim=1.2),peatland_costsGLO)
    }
  }

  totCosts <- dimSums(totCosts, dim = 3)

  return(totCosts)

}
