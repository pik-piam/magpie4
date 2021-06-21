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

  # Factor to evaluate costs as they enter the optimization or including total investments
  f_an <- 1

  if (type == "investment") {

    int_rate <- int_rate <- readGDX(gdx, "pm_interest")[, readGDX(gdx, "t"), ]
    t <- getYears(int_rate, as.integer = TRUE)
    t_step <- c(t[seq_len(length(t))[2:length(t)]], 2110) - t
    t_sm <- int_rate

    for (y in seq_len(length(getYears(t_sm)))) {
      t_sm[, y, ] <- t_step[y]
    }

    f_an <- (1 + int_rate) / (int_rate) / t_sm

  }

  # Production
  production <- production(gdx, level = "reg", products = "kcr", product_aggr = TRUE)

  # Input factor costs crops
  IFC <- superAggregate(CostInputFactorsCrop(gdx, type = "annuity", level = "reg"),
    aggr_type = "sum", level = "reg") / production * f_an
  getNames(IFC) <- "Inputs (Crops)"

  # TC
  int_rate <- readGDX(gdx, "pm_interest")[, getYears(IFC), ]
  TC <- superAggregate(collapseNames(readGDX(gdx, "ov_tech_cost")[, , "level"]),
    aggr_type = "sum", level = "reg") / production * f_an
  getNames(TC) <- "TC"
  # Irrigation
  AEI <- superAggregate(collapseNames(readGDX(gdx, "ov_cost_AEI")[, , "level"]),
    aggr_type = "sum", level = "reg") / production * f_an
  getNames(AEI) <- "AEI"


  # Trade
  kcr <- findset("kcr")
  int_kcr <- intersect(kcr, getNames(collapseNames(readGDX(gdx, "ov21_cost_trade_reg")[, , "level"])))
  Trade <- superAggregate(dimSums(collapseNames(readGDX(gdx, "ov21_cost_trade_reg")[, , "level"][, , int_kcr]),
    dim = 3), aggr_type = "sum", level = "reg") / production
  getNames(Trade) <- "Trade (Crops)"

  # Land Conversion
  LC <- superAggregate(collapseNames(readGDX(gdx, "ov_cost_landcon")[, , "level"][, , "crop"]),
    aggr_type = "sum", level = "reg") / production * f_an
  getNames(LC) <- "Land conversion costs (Cropland)"

  out <- mbind(IFC, TC, AEI, Trade, LC)

  out <- if (!(level %in% c("reg", "regglo"))) gdxAggregate(gdx, out, weight = NULL,
                                                            absolute = FALSE, to = level, dir = dir) else out

  out <- if (level == "regglo") mbind(out, gdxAggregate(gdx, out, weight = production,
                                                       absolute = FALSE, to = "glo")) else out


  out(out, file)
}
