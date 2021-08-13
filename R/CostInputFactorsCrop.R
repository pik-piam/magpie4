#' @title costInputFactorsCrop
#' @description Reads data to calculate Input factors with different approaches
#'
#' @export
#'
#' @param gdx GDX file
#' @param type Type of capital investments accounting. It can either be total investments ("investment"),
#' or considering the annuity ("annuity") of the current time step. NULL in case the runs were not done with
#' the sticky realization.
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
#' x <- costInputFactorsCrop(gdx)
#' }
#'
costInputFactorsCrop <- function(gdx, type = "annuity", file = NULL, level = "reg") {

  if (suppressWarnings(is.null(readGDX(gdx, "p38_capital_mobile")))) {

    if (is.null(type)) {
      kcr <- findset("kcr")
      out <- dimSums(collapseNames(readGDX(gdx, "ov_cost_prod")[, , "level"][, , kcr]), dim = 3)
      getNames(out) <- "Variable costs for crops"
    } else {
      stop("Selected type not available for runs done without the sticky realization of the factor costs")
    }

  } else {

    if (!is.null(type)) {
      if (type == "annuity") {

        kcr <- findset("kcr")
        Variable <- setNames(dimSums(collapseNames(readGDX(gdx, "ov_cost_prod")[, , "level"][, , kcr]), dim = 3),"Labor costs for crops")
        Investments <-setNames(collapseNames(readGDX(gdx, "ov_cost_inv")[, , "level"]),"Investment costs for crops (annuity)")

        out<-mbind(Variable,Investments)

      } else if (type == "investment") {

        kcr <- findset("kcr")
        variable <- setNames(dimSums(collapseNames(readGDX(gdx, "ov_cost_prod")[, , "level"][, , kcr]), dim = 3),"Labor costs for crops")
        capital <-  setNames(CostCapital(gdx, type = "investment", level = "reg"),"Investment costs for crops (sunk)")
        out <- mbind(variable,capital)
      }
    } else {
      stop("Type not existent for sticky runs")
    }

  }


  if (level %in% c("glo", "regglo")) out <- superAggregate(out, aggr_type = "sum", level = level)


  out(out, file)
}
