#' @title NetForestChange
#' @description Calculates net and gross forest area change based on a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param lowpass number of lowpass filter iterations (default = NULL)
#' @return Net Forest Change as MAgPIE object (Mha per year)
#' @author Florian Humpenoeder
#' @importFrom magclass new.magpie getCells lowpass setNames getNames getYears setYears
#' @examples
#' \dontrun{
#' x <- NetForestChange(gdx)
#' }
#'
NetForestChange <- function(gdx, file = NULL, level = "cell", lowpass = NULL) {

  # get year diff
  im_years <- collapseNames(m_yeardiff(gdx))
  # function for conversion from stocks to annual flows
  calcAnnual <- function(a) {
    # no data for 1st year
    t1 <- a[, 1, ]
    t1[, , ] <- NA
    # calc diff between land stocks
    a <- as.magpie(apply(a, c(1, 3), diff))
    # divide by time step length
    a <- a / im_years[, getYears(a), ]
    # add 1st year
    a <- mbind(t1, a)
    return(a)
  }

  ### forestry
  a <- dimSums(readGDX(gdx,"ov32_land", select = list(type="level")),dim="ac")
  a <- calcAnnual(a)
  forestryExp <- readGDX(gdx,"ov_landexpansion_forestry", select = list(type="level"))
  forestryRed <- readGDX(gdx,"ov_landreduction_forestry", select = list(type="level"))
  forestryExp <- forestryExp / im_years[, getYears(forestryExp), ]
  forestryRed <- forestryRed / im_years[, getYears(forestryRed), ]
  forestryNet <- forestryExp - forestryRed
  if(any(abs(forestryNet[,-1]-a[,-1]) > 10^-6)) warning("Forestry: Sum of gross changes does not match net change")

  ### secondary forest
  a <- dimSums(readGDX(gdx,"ov35_secdforest", select = list(type="level")),dim="ac")
  a <- calcAnnual(a)
  secdfExp <- readGDX(gdx,"ov35_secdforest_expansion", select = list(type="level")) + dimSums(readGDX(gdx,"p35_maturesecdf"),dim=3) + dimSums(readGDX(gdx,"p35_disturbance_loss_primf"),dim=3) + dimSums(readGDX(gdx,"p35_disturbance_loss_secdf"),dim=3)
  secdfRed <- dimSums(readGDX(gdx,"ov35_secdforest_reduction", select = list(type="level")),dim=3) + dimSums(readGDX(gdx,"p35_disturbance_loss_secdf"),dim=3)
  secdfExp <- secdfExp / im_years[, getYears(secdfExp), ]
  secdfRed <- secdfRed / im_years[, getYears(secdfRed), ]
  secdfNet <- secdfExp - secdfRed
  if(any(abs(secdfNet[,-1]-a[,-1]) > 10^-6)) warning("Secondary Forest: Sum of gross changes does not match net change")

  ### primary forest
  a <- land(gdx,types = "primforest", level = "cell")
  a <- calcAnnual(a)
  primfRed <- dimSums(readGDX(gdx,"ov35_primforest_reduction", select = list(type="level")),dim=3) + dimSums(readGDX(gdx,"p35_disturbance_loss_primf"),dim=3)
  primfExp <- primfRed
  primfExp[,,] <- 0
  primfExp <- primfExp / im_years[, getYears(primfExp), ]
  primfRed <- primfRed / im_years[, getYears(primfRed), ]
  primfNet <-  primfExp - primfRed
  if(any(abs(primfNet[,-1]-a[,-1]) > 10^-6)) warning("Primary Forest: Sum of gross changes does not match net change")

  ### combine
  a <- mbind(setNames(forestryExp[,,"plant"], "plantExp"),
             setNames(forestryRed[,,"plant"], "plantRed"),
             setNames(forestryNet[,,"plant"], "plantNet"),
             setNames(forestryExp[,,"ndc"], "ndcExp"),
             setNames(forestryRed[,,"ndc"], "ndcRed"),
             setNames(forestryNet[,,"ndc"], "ndcNet"),
             setNames(forestryExp[,,"aff"], "affExp"),
             setNames(forestryRed[,,"aff"], "affRed"),
             setNames(forestryNet[,,"aff"], "affNet"),
             setNames(secdfExp, "secdfExp"),
             setNames(secdfRed, "secdfRed"),
             setNames(secdfNet, "secdfNet"),
             setNames(primfExp, "primfExp"),
             setNames(primfRed, "primfRed"),
             setNames(primfNet, "primfNet"))

  # years
  years <- getYears(a, as.integer = T)
  yr_hist <- years[years > 1995 & years <= 2025]
  yr_fut <- years[years >= 2025]

  # apply lowpass filter (not applied on 1st time step, applied seperatly on historic and future period)
  if (!is.null(lowpass)) a <- mbind(a[, 1995, ], lowpass(a[, yr_hist, ], i = lowpass), lowpass(a[, yr_fut, ], i = lowpass)[, -1, ])

  # aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level, na.rm = FALSE)

  out(a, file)
}
