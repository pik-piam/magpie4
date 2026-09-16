#' @title discountRates
#' @description reads discount rates from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing discount rates used in the model
#' @author Xiaoxi Wang
#' @examples
#'
#'   \dontrun{
#'     x <- discountRates(gdx)
#'   }
#'
discountRates <- function(gdx, file = NULL, level = "reg") {
  x <- readGDX(gdx, "pm_interest", format = "first_found")
  weight <- income(gdx, per_capita = FALSE)
  if (!is.null(getYears(x))) {
    x <- x[, getYears(weight), ]
  }
  weight <- apply(weight[, (1:3), ], 1, mean)
  weight <- as.magpie(weight)
  x <- superAggregate(x, aggr_type = "weighted_mean", level = level, weight = weight)
  out(x, file)
}
