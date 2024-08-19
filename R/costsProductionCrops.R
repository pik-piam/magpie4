#' @title costsProductionCrops
#' @description Reads data to calculate production costs for crops, costs related with
#' investments are reported as annual average for both types (annuity, investment).
#' @export
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in gdxAggregate
#' @param type Type of reporting, either "annuity" or total "investments"
#' @param dir directory with mapping for disaggregation to higher resolutions
#' @return A MAgPIE object containing values related with costs for crops production
#' per ton produced [million US$17/tDM]
#' @author Edna Molina Bacca
#' @importFrom magclass mbind dimSums collapseNames
#' @importFrom magpiesets findset
#' @importFrom madrat toolAggregate
#' @examples
#' \dontrun{
#' x <- costsProductionCrops(gdx)
#' }
#'
costsProductionCrops <- function(gdx, file = NULL, level = "regglo", type = "investment", dir = ".") {

   int_rate <- readGDX(gdx, "pm_interest")[, readGDX(gdx, "t"), ]
   supreg <- readGDX(gdx, "supreg", react = "silent")
   if (!is.null(supreg) && all(supreg$i %in% getCells(int_rate))) {
     weight <- int_rate
     weight[, , ] <- 1
     int_rate <- toolAggregate(int_rate, supreg, weight = weight, from = "i", to = "h")
   }

   factorInvestment <- if (type == "investment") (int_rate) / (1 + int_rate) else 1


     t <- getYears(int_rate, as.integer = TRUE)
     t_step <- t - c(1990, t[seq_len(length(t))[1:(length(t) - 1)]])
     t_sm <- int_rate
     for (y in seq_len(length(getYears(t_sm)))) {
       t_sm[, y, ] <- t_step[y]
     }



   factor <- if (type == "investment") factorInvestment * t_sm else t_sm

  # Input factor costs crops

  if (suppressWarnings(is.null(readGDX(gdx, "p38_capital_mobile")))) {
    IFC <- costInputFactorsCrop(gdx, type = NULL, level = "reg")
    getNames(IFC) <- c("Input costs (Crops)")
  } else {
    IFC <- costInputFactorsCrop(gdx, type = type, level = "reg")
    IFC <- if (!is.null(supreg) && all(supreg$i %in% getCells(IFC))) toolAggregate(IFC, supreg)
    IFC[, , 2] <- IFC[, , 2] / factor
    getNames(IFC) <- c("Labor (Crops)", "Capital (Crops)")

    IFC <- add_columns(IFC, addnm = "Input costs (Crops)", dim = 3.1)
    IFC[, , "Input costs (Crops)"] <- dimSums(IFC[, , c("Labor (Crops)", "Capital (Crops)")], dim = 3.1)
  }


  # Trade

  Trade <- readGDX(gdx, "ov21_cost_trade_reg", select = list(type = "level"), react = "silent")
  if(!is.null(Trade)) {
    kcr <- findset("kcr")
    intKcr <- intersect(kcr, getNames(Trade))
    Trade <- setNames(dimSums(Trade[, , intKcr], dim = 3), "Trade (Crops)")
  }

  # TC,AEI and Land conversion can be read from the costs function

  CO_costs <- setNames(costs(gdx, level = "reg",
                    type = type, sum = FALSE)[, , c("TC", "AEI", "Land Conversion")],
                    c("Technology", "AEI", "Land Conversion"))

  CO_costs <- if (!is.null(supreg) && all(supreg$i %in% getCells(CO_costs))) toolAggregate(CO_costs, supreg)
  CO_costs <- CO_costs / factor

  out <- mbind(IFC, Trade, CO_costs)
  out <- if (!(level %in% c("reg", "regglo", "glo"))) gdxAggregate(gdx, out, weight = "croparea",
                                                            absolute = TRUE, to = level, dir = dir) else out

  out <- if (level == "regglo") mbind(out, setCells(dimSums(out, dim = 1), "GLO")) else out
  out <- if (level == "glo") setCells(dimSums(out, dim = 1), "GLO") else out


  out(out, file)
}
