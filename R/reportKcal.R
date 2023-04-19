#' @title reportKcal
#' @description reports per-capita calories food supply (including household waste)

#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return per-capita calories as MAgPIE object (kcal/cap/day)
#' @author Benjamin Leon Bodirsky, Kristine karstens, Abhijeet Mishra
#' @examples
#'
#'   \dontrun{
#'     x <- reportKcal(gdx)
#'   }
#'

reportKcal<-function(gdx,detail=FALSE,level="regglo"){

  level_zero_name <- "Nutrition|Calorie Supply"

  out<-Kcal(gdx,level = level, products = "kall",product_aggr = FALSE, calibrated=TRUE,magpie_input = FALSE)

  out<-reporthelper(x=out,level_zero_name = level_zero_name,detail = detail)

  if(level_zero_name%in%getNames(out)){
    sumup  <- getNames(out[,,level_zero_name,invert=TRUE])
    getNames(out)  <- c(level_zero_name,getNames(summationhelper(out[,,sumup],sep="+", dim=3.1)))
  } else {getNames(out) <- getNames(summationhelper(out, sep="+", dim=3.1))}

  getNames(out) <- paste(getNames(out),"(kcal/capita/day)",sep=" ")

  #delete empty categories
  out<-out[,,getNames(out)[which(dimSums(out,dim=c(1,2))!=0)]]
  return(out)
}
