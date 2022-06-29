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
                  )
  
  totCosts <- costsOptimization(gdx = gdx, level = level, type = "investment", sum = FALSE ) #use costsOptimization investment type (costs are one-off at that time step, not amortized)
  totCosts <- totCosts[,,-which(getNames(totCosts) %in% incentives)] #take out those incentives that are present
  
  #peatland costs without slack are in v58_peatland_cost
  peatland_costs <- readGDX(gdx,"ov58_peatland_cost",select = list(type="level"),react = "silent")
  totCosts <- add_columns(totCosts, addnm = "Peatland", dim = 3.1, fill = 0)
  
  if(!is.null(peatland_costs)) {
  totCosts[,,"Peatland"] <- mbind(superAggregate(peatland_costs, aggr_type = "sum", level = "reg"),
                                  superAggregate(peatland_costs, aggr_type = "sum", level = "glo"))
  } 
  
  totCosts <- dimSums(totCosts, dim = 3)
  
}
