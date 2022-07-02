#' @title reportIntakeDetailed
#' @description reports detailed or aggregated per-capita kcal intake including exogenous scenarios

#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return per-capita calorie intake as MAgPIE object (kcal/cap/day)
#' @author Isabelle Weindl
#' @examples
#'
#'   \dontrun{
#'     x <- reportIntakeDetailed(gdx)
#'   }
#'

reportIntakeDetailed<-function(gdx,detail=TRUE,level="regglo"){

  level_zero_name <- "Nutrition|Calorie Intake"

  out<- IntakeDetailed(gdx,level = level,product_aggr = FALSE)

  out<-reporthelper(x=out,level_zero_name = level_zero_name,detail = detail,partly=TRUE)

  if(level_zero_name%in%getNames(out)){
    sumup  <- getNames(out[,,level_zero_name,invert=TRUE])
    getNames(out)  <- c(level_zero_name,getNames(summationhelper(out[,,sumup],sep="+", dim=3.1)))
  } else {getNames(out) <- getNames(summationhelper(out, sep="+", dim=3.1))}

  getNames(out) <- paste(getNames(out),"(kcal/capita/day)",sep=" ")

  #delete empty categories
  out<-out[,,getNames(out)[which(dimSums(out,dim=c(1,2))!=0)]]
  return(out)
}
