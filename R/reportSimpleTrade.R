#' @title reportSimpleTrade
#' @description reports a simple trade metric (differences between demand and production)
#'
#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param level Aggregation level at which the data should be reported
#' @param detail if true, provides estimates for all commodities, otherwise aggregates some groups
#' @return Net-Exports as MAgPIE object. Unit: see names
#' @author Patrick Rein, Mishko Stevanovic
#' @examples
#'
#'   \dontrun{
#'     x <- reportSimpleTrade(gdx="fulldata.gdx")
#'   }
#' @importFrom magpiesets findset

reportSimpleTrade <- function(gdx, detail = FALSE, level = "regglo") {

  x <- NULL

  production <- production(gdx, level = "reg", products = "kall",
                           product_aggr = FALSE)

  demand <- dimSums(demand(gdx, level = "reg", products = "kall",
                           product_aggr = FALSE),
                    3.1)

  .convertToReportFormat <- function(magpieObject, levelName) {
    out <- reporthelper(x = magpieObject, dim = 3.1,
                        level_zero_name = levelName,
                        detail = detail, partly = TRUE)
    getNames(out) <- paste(getNames(out), "(Mt DM/yr)", sep = " ")
    out <- summationhelper(out, excludeLevels = 1)
    return(out)
  }

  # First do regional computations, then aggregate
  # to potential larger aggregations

  # Calculate export
  export <- production - demand
  export[export < 0] <- 0
  export <- .convertToReportFormat(export, "Trade|Exports")

  # Calculate import
  import <- demand - production
  import[import < 0] <- 0
  import <- .convertToReportFormat(import, "Trade|Imports")

  # Finalize report data
  x <- mbind(export, import)
  x <- superAggregateX(x, "sum", level = level)

  return(x)
}