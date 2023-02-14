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
  totCosts <- totCosts[, , -which(getNames(totCosts) %in% incentives |
                                    getNames(totCosts) %in% c("Forestry", "Timber production"))]
  # remove wage rent from input factor costs
  totCosts[, , "Input Factors"] <- totCosts[, , "Input Factors"] - wageRent(gdx = gdx, level = level)
  # remove tax Revenue from input factor costs
  totCosts[, , "Penalty or tax for violating crop rotations"] <- totCosts[, ,
                                                                 "Penalty or tax for violating crop rotations"] -
                                                                  taxRevenueRotations(gdx = gdx, level = level)


  # peatland costs without slack are in v58_peatland_cost
  peatlandCosts <- readGDX(gdx, "ov58_peatland_cost", select = list(type = "level"), react = "silent")
  totCosts <- add_columns(totCosts, addnm = "Peatland", dim = 3.1, fill = 0)

  if (!is.null(peatlandCosts)) {
    if (level == "reg") {
      totCosts[, , "Peatland"] <- dimSums(peatlandCosts, dim = 1.2)
    } else if (level == "glo") {
      peatlandCosts <- dimSums(peatlandCosts, dim = 1)
      getItems(peatlandCosts, dim = 1) <- "GLO"
      totCosts[, , "Peatland"] <- dimSums(peatlandCosts, dim = 1)
    } else if (level == "regglo") {
      peatlandCostsGLO <- dimSums(peatlandCosts, dim = 1)
      getItems(peatlandCostsGLO, dim = 1) <- "GLO"
      totCosts[, , "Peatland"] <- mbind(dimSums(peatlandCosts, dim = 1.2), peatlandCostsGLO)
    }
  }

  totCosts <- dimSums(totCosts, dim = 3)

  return(totCosts)

}
