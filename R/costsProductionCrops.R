#' @title costsProductionCrops
#' @description Reads data to calculate production costs for crops
#' @export
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param type Type of reporting, either "annuity" or total "investments"
#' @param dir directory with mapping for disaggregation to higher resolutions
#' @return A MAgPIE object containing values related with costs for crops production
#' per ton produced [million US$05/tDM]
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom luscale superAggregate
#' @importFrom magclass mbind dimSums collapseNames
#' @examples
#' \dontrun{
#' x <- costsProductionCrops(gdx)
#' }
#'
costsProductionCrops <- function(gdx, file = NULL, level = "regglo", type = "investment", dir = ".") {


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

  if (suppressWarnings(is.null(readGDX(gdx, "p38_capital_mobile")))) {
    IFC <- costInputFactorsCrop(gdx, type = NULL, level = "reg")
    getNames(IFC) <- c("Input costs (Crops)")
  } else {
    IFC <- costInputFactorsCrop(gdx, type = type, level = "reg")
    IFC[, , 2] <- IFC[, , 2] / t_sm
    getNames(IFC) <- c("Variable (Crops)", "Capital (Crops)")

    IFC <- add_columns(IFC, addnm = "Input costs (Crops)", dim = 3.1)
    IFC[, , "Input costs (Crops)"] <- dimSums(IFC[, , c("Variable (Crops)", "Capital (Crops)")], dim = 3.1)
  }



  # Trade
  Trade <- readGDX(gdx, "ov_cost_trade", select=list(type="level"))
  getNames(Trade) <- "Trade (Crops)"

  # TC,AEI and Land conversion can be read from the costs function

  CO_costs <- setNames(costs(gdx, level = "reg",
                    type = type, sum = FALSE)[, , c("TC", "AEI", "Land Conversion")],c("Technology", "AEI", "Land Conversion"))

  out <- mbind(IFC, Trade, CO_costs)



  out <- if (!(level %in% c("reg", "regglo"))) gdxAggregate(gdx, out, weight = "croparea",
                                                            absolute = TRUE, to = level, dir = dir) else out

  out <- if (level == "regglo") mbind(out, gdxAggregate(gdx, out, weight = NULL,
                                                        absolute = TRUE, to = "glo")) else out

  out(out, file)
}
