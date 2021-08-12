#' @title costsAdaptationCrops
#' @description Reads data to calculate adaptation costs for cropland
#' @export
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param type Type of reporting, either "annuity" or total "investments"
#' @param dir directory with mapping for disaggregation to higher resolutions
#' @return A MAgPIE object containing values related with adaptation costs for crops production
#' per ton produced [million US$05/tDM]
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom luscale superAggregate
#' @importFrom magclass mbind dimSums collapseNames
#' @examples
#' \dontrun{
#' x <- costsAdaptationCrops(gdx)
#' }
#'
costsAdaptationCrops <- function(gdx, file = NULL, level = "regglo", type = "investment", dir = ".") {


  t_sm <- 1

  if (type == "investment") {

    int_rate <- int_rate <- readGDX(gdx, "pm_interest")[, readGDX(gdx, "t"), ]
    t <- getYears(int_rate, as.integer = TRUE)
    t_step <- t - c(1990, t[seq_len(length(t))[1:(length(t) - 1)]])
    t_sm <- int_rate

    for (y in seq_len(length(getYears(t_sm)))) {
      t_sm[, y, ] <- t_step[y]
    }
  }


  # Input factor costs crops

  if (suppressWarnings(is.null(readGDX(gdx, "p38_capital_mobile_t")))) {
    IFC <- costInputFactorsCrop(gdx, type = NULL, level = "reg")
    getNames(IFC) <- c("Input costs (Crops)")
  }else{
    IFC <- costInputFactorsCrop(gdx, type = type, level = "reg")
    IFC[,,2]<-IFC[,,2] / t_sm
    getNames(IFC) <- c("Labor (Crops)","Capital (Crops)")
  }
  
  

  # Trade
  kcr <- findset("kcr")
  int_kcr <- intersect(kcr, getNames(collapseNames(readGDX(gdx, "ov21_cost_trade_reg")[, , "level"])))
  Trade <- superAggregate(dimSums(collapseNames(readGDX(gdx, "ov21_cost_trade_reg")[, , "level"][, , int_kcr]),
    dim = 3), aggr_type = "sum", level = "reg")
  getNames(Trade) <- "Trade (Crops)"

  # TC,AEI and Land conversion can be read from the costsOptimization function

  CO_costs <- costsOptimization(gdx, level = "reg",
                                type = type, sum = FALSE)[, , c("Technology", "AEI", "Land Conversion")]

  out <- mbind(IFC, Trade, CO_costs)

 

  out <- if (!(level %in% c("reg", "regglo"))) gdxAggregate(gdx, out, weight = "croparea",
                                                            absolute = TRUE, to = level, dir = dir) else out

  out <- if (level == "regglo") mbind(out, gdxAggregate(gdx, out, weight = NULL,
                                                       absolute = TRUE, to = "glo")) else out

  out(out, file)
}
