#' @title reportLivestockShare
#' @description reports the share of livestock products (including fish) in total calorie food supply
#'
#' @export
#'
#' @param gdx GDX file
#' @return per-capita calories as MAgPIE object (cal/cap/day)
#' @author Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportLivestockShare(gdx)
#'   }
#'

reportLivestockShare<-function(gdx){
  out<-Kcal(gdx,level = "regglo",products = "kall",product_aggr = FALSE)
  kap<-findset("kap")
  l = dimSums(out[,,kap],dim=3.1)
  weight = dimSums(out,dim=3.1)
  out<-l/weight
  getNames(out) <- "Nutrition|Dietary Composition|Livestock Share (cal/cal)"

  return(out)
}
