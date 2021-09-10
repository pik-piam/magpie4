#' @title NetForestChange
#' @description Calculates net forest change based on a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param lowpass number of lowpass filter iterations (default = 3)
#' @return Net Forest Change as MAgPIE object (Mha per year)
#' @author Florian Humpenoeder
#' @importFrom magclass new.magpie getCells lowpass setNames getNames getYears setYears
#' @examples
#' \dontrun{
#' x <- NetForestChange(gdx)
#' }
#'
NetForestChange <- function(gdx, file = NULL, level = "cell", lowpass = 3) {
  # get year diff
  im_years <- collapseNames(m_yeardiff(gdx))
  # get forest area
  a <- dimSums(land(gdx, types = c("primforest", "secdforest", "forestry")), dim = 3)
  # no data for 1st year
  t1 <- a[, 1, ]
  t1[, , ] <- NA
  # calc diff between land stocks
  a <- as.magpie(apply(a, c(1, 3), diff))
  # divide by time step length
  a <- a / im_years[, getYears(a), ]
  # add 1st year
  a <- mbind(t1, a)

  # years
  years <- getYears(a, as.integer = T)
  yr_hist <- years[years > 1995 & years <= 2020]
  yr_fut <- years[years >= 2020]

  # apply lowpass filter (not applied on 1st time step, applied seperatly on historic and future period)
  if (!is.null(lowpass)) a <- mbind(a[, 1995, ], lowpass(a[, yr_hist, ], i = lowpass), lowpass(a[, yr_fut, ], i = lowpass)[, -1, ])

  # aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level, na.rm = FALSE)

  out(a, file)
}
