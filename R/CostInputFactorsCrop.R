#' @title CostInputFactorsCrop
#' @description Reads data to calculate Input factors with different approaches
#'
#' @export
#'
#' @param gdx GDX file
#' @param type Type of capital investments accounting. It can either be overall ("overall") investments, or considering the annuity ("annuity") of the current time step. NULL in case the runs were not done with the sticky realization.
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation
#' @return A MAgPIE object containing values related with overall value of production [million US$05]
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom luscale superAggregate
#' @importFrom magpiesets findset
#'
#' @examples
#' \dontrun{
#' x <- CostInputFactorsCrop(gdx)
#' }
#'
CostInputFactorsCrop <- function(gdx, type = "annuity", file = NULL, level = "cell") {

  if (suppressWarnings(is.null(readGDX(gdx, "ov_cost_inv")))) {

    if (is.null(type)) {
      kcr <- findset("kcr")
      out <- dimSums(collapseNames(readGDX(gdx, "ov_cost_prod")[, , "level"][, , kcr]),dim=3)
      getNames(out) <- "Input costs for crops"
    } else {
      stop("Selected type not available for runs done without the sticky realization of the factor costs")
    }

  } else {

    if (!is.null(type)) {
      if (type == "annuity") {

        kcr <- findset("kcr")
        out <- dimSums(collapseNames(readGDX(gdx, "ov_cost_prod")[, , "level"][, , kcr]), dim = 3) + 
          collapseNames(readGDX(gdx, "ov_cost_inv")[, , "level"])
        getNames(out) <- "Input costs for crops (Capital Annuity)"

      } else if (type == "overall") {

        kcr <- findset("kcr")
        variable <- dimSums(collapseNames(readGDX(gdx, "ov_cost_prod")[, , "level"][, , kcr]), dim = 3)
        capital <- CostCapital(gdx, type = "investment", level = "cell")
        out <- variable + capital

        getNames(out) <- "Input costs for crops (Sunk capital)"
      }
    } else {
      stop ("Type not existent for sticky runs")
    }

  }


  if (level != "cell") out <- superAggregate(out, aggr_type = "sum", level = level)


  out(out, file)
}
