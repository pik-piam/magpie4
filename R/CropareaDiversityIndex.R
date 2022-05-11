#' @title CropareaDiversityIndex
#' @description calculates an index that measures the gini for croparea diversity
#'
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#'              "regglo" (regional and global) or
#'              any other aggregation level defined in superAggregate
#' @param measurelevel level at which diversity is measured. "cell" means diversity
#' is measured at cellular level, even if lateron average diversity is aggregated
#' to regional level.
#' @return MAgPIE object (unit depends on attributes)
#' @author Benjamin Leon Bodirsky
#' @seealso \code{\link{CropareaDiversityIndex}}
#' @examples
#' \dontrun{
#' x <- CropareaDiversityIndex(gdx)
#' }
#'
CropareaDiversityIndex <- function(gdx, level = "reg", measurelevel="cell") {

  #dir <- getDirectory(dir, spamfiledirectory)

  area = croparea(gdx=gdx, level = measurelevel, product_aggr = FALSE)
  land = land(gdx = gdx, level = measurelevel, types = "crop")
  area = mbind(area, setNames(land-dimSums(area,dim=3),"fallow"))

  ### honor to function in dineq:::gini.wtd !
  gini.wtd = function (x, weights = NULL)  {
    if (is.null(weights)) {
      weights <- rep(1, length(x))
    }
    missing <- !(is.na(x) | is.na(weights))
    x <- x[missing]
    weights <- weights[missing]
    if (!all(weights >= 0))
      stop("At least one weight is negative", call. = FALSE)
    if (all(weights == 0))
      stop("All weights are zero", call. = FALSE)
    weights <- weights/sum(weights)
    order <- order(x)
    x <- x[order]
    weights <- weights[order]
    p <- cumsum(weights)
    nu <- cumsum(weights * x)
    n <- length(nu)
    nu <- nu/nu[n]
    gini <- sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
    return(gini)
  }

  cropdiv=function(cellvalue,cropnames){
    cellvalue=as.vector(cellvalue)
    names(cellvalue)=cropnames
    single=c("maiz", "trce", "rice_pro", "soybean", "rapeseed", "groundnut",
    "sunflower", "oilpalm", "potato", "sugr_cane", "sugr_beet",
    "cottn_pro", "begr")
    # weights could be improved
    mix <- c(
      cellvalue[single],
      rep(cellvalue["foddr"]/5, 5),
      rep(cellvalue["tece"]/3, 3),
      rep(cellvalue["puls_pro"]/3, 3),
      rep(cellvalue["betr"]/2, 2),
      rep(cellvalue["cassav_sp"]/2, 2),
      rep(cellvalue["others"]/10, 10)
    )
    gini <- gini.wtd(mix)
    return(gini)
  }
  x=magpply(area,FUN = cropdiv,DIM=3,cropnames=getNames(area))
  x[is.na(x)]=1


  out <- gdxAggregate(gdx, x, to = level,
                      weight = "land", type = "crop", absolute = FALSE,
                      dir = dir)
  return(out)
}
