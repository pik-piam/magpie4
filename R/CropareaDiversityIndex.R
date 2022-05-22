#' @title CropareaDiversityIndex
#' @description calculates an index that measures the croparea diversity
#'
#' @export
#'
#' @param gdx GDX file
#' @param index can be "shannon", "gini" or "invsimpson" for different types of diversitiy indices
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
CropareaDiversityIndex <- function(gdx,index="shannon", level = "reg", measurelevel="cell", groupdiv=TRUE) {

  #dir <- getDirectory(dir, spamfiledirectory)

  area = croparea(gdx=gdx, level = measurelevel, product_aggr = FALSE)
  land = land(gdx = gdx, level = measurelevel, types = "crop")
  area = mbind(area, setNames(land-dimSums(area,dim=3),"fallow"))

  ### honor to function dineq:::gini.wtd !
  gini = function (x)  {
    weights <- rep(1, length(x))
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
  ### honor to function vegan:::diversity !
  shannon=function(x,base = exp(1)){
    x <- x/sum(x)
    x <- -x * log(x, base)
    H <- sum(x, na.rm = TRUE)
    return(H)
  }
  invsimpson=function(x){
    x <- x/sum(x)
    x <- x * x
    H <- sum(x, na.rm = TRUE)
    H <- 1/H
    return(H)
  }

  selection<-function(x,index){
    if(index=="shannon"){
      x=shannon(x)
    }else if (index=="invsimpson"){
      x=invsimpson(x)
    }else if (index=="gini"){
      x=gini(x)
    } else {stop("unknown index")}
    return(x)
  }

  cropdiv=function(cellvalue,cropnames){
    cellvalue=as.vector(cellvalue)
    names(cellvalue)=cropnames
    single=c("maiz", "trce", "rice_pro", "soybean", "rapeseed", "groundnut",
    "sunflower", "oilpalm", "potato", "sugr_cane", "sugr_beet",
    "cottn_pro", "begr")
    # weights could be improved
    if(groupdiv){
      mix <- c(
        cellvalue[single],
        rep(cellvalue["foddr"]/4, 4),
        rep(cellvalue["tece"]/2, 2),
        rep(cellvalue["puls_pro"]/3, 3),
        rep(cellvalue["betr"]/2, 2),
        rep(cellvalue["cassav_sp"]/2, 2),
        rep(cellvalue["fallow"]/4, 4),
        rep(cellvalue["others"]/10, 10)
      )
    } else {mix=cellvalue}
    gini <- selection(mix,index)

    return(gini)
  }
  x=magpply(area,FUN = cropdiv,DIM=3,cropnames=getNames(area))
  if(index=="gini"){
    x[is.na(x)]=1
    x[x==Inf]=1
  }else{
    x[is.na(x)]=0
    x[x==Inf]=0
  }


  out <- gdxAggregate(gdx, x, to = level,
                      weight = "land", type = "crop", absolute = FALSE,
                      dir = ".")
  return(out)
}
