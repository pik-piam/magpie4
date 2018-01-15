#' @title reportVegfruitShare
#' @description reports the share of livestock products (including fish) in total calorie food supply
#' 
#' @export
#'
#' @param gdx GDX file
#' @return per-capita calories as MAgPIE object (kcal/cap/day)
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportLivestockShare(gdx)
#'   }
#' 

reportVegfruitShare<-function(gdx){
  out<-Kcal(gdx,level = "regglo",products = "kall",product_aggr = FALSE)
  l = dimSums(out[,,"others"],dim=3.1)
  weight = dimSums(out,dim=3.1) 
  out<-l/weight
  getNames(out) <- "Nutrition|Dietary Composition|Vegetables Fruits Nuts Share (kcal/kcal)"
  
  return(out)
}
