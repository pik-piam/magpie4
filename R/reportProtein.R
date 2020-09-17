#' @title reportProtein
#' @description reports per-capita protein food supply (including household waste)

#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return per-capita protein as MAgPIE object (protein/cap/day)
#' @author Benjamin Leon Bodirsky, Kristine Karstens, Abhijeet Mishra, Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportKcal(gdx)
#'   }
#' 

reportProtein<-function(gdx,detail=FALSE){

  level_zero_name <- "Nutrition|Protein Supply"
  
  out<-collapseNames(Kcal(gdx,level = "regglo", products = "kall",product_aggr = FALSE, calibrated=TRUE,magpie_input = TRUE,attributes="protein"),collapsedim = 2)
  
  out<-reporthelper(x=out,level_zero_name = level_zero_name,detail = detail)
  
  if(level_zero_name%in%getNames(out)){
    sumup  <- getNames(out[,,level_zero_name,invert=TRUE])
    getNames(out)  <- c(level_zero_name,getNames(summationhelper(out[,,sumup],sep="+", dim=3.1)))
  } else {getNames(out) <- getNames(summationhelper(out, sep="+", dim=3.1))}
  
  getNames(out) <- paste(getNames(out),"(protein/capita/day)",sep=" ") 
  
  #delete empty categories
  out<-out[,,getNames(out)[which(dimSums(out,dim=c(1,2))!=0)]]
  return(out)
}
