#' @title CostCapital
#' @description Reads data to calculate capital stocks
#'
#' @param gdx GDX file
#' @param type either capital stocks ("stocks") or overall capital investment "investment"
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing values related with overall value of production [million US$17]
#' @author Edna Molina Bacca
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- CostCapital(gdx)
#'   }
#' @export
CostCapital <- function(gdx, type = "stocks", file = NULL, level = "cell") {

  if (type == "stocks") {
    #Reads existing capital in each time step
    capital_im <- suppressWarnings(readGDX(gdx, "p38_capital_immobile"))
    if (!is.null(capital_im)) {
      capital_im <- dimSums(capital_im, dim = 3)
    }
    capital_mo <- suppressWarnings(readGDX(gdx, "p38_capital_mobile"))
    tag <- "Capital Stocks"
  } else if (type == "investment") {
    capital_im <- suppressWarnings(readGDX(gdx, "ov38_investment_immobile")[, , "level"])
    if (!is.null(capital_im)) {
      capital_im <- dimSums(collapseNames(capital_im), dim = 3)
    }
    capital_mo <- suppressWarnings(readGDX(gdx, "ov38_investment_mobile")[, , "level"])
    if (!is.null(capital_mo)) {
      capital_mo <- collapseNames(capital_mo)
    }
    tag <- "Capital Investments"
  }

  # PerTon factor costs realizations don't contain capital info.
  # Check that stops the function in case capital is not accounted for
  if (any(is.null(capital_im), is.null(capital_mo))) {
    stop("Capital information only available for the sticky factor costs realization")
  }

  sumCap <- capital_im + capital_mo
  getNames(sumCap) <- tag
  out <- sumCap

  if (level != "cell") {
    out <- superAggregateX(out, aggr_type = "sum", level = level)
  }

  out(out, file)
}
