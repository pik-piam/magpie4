#' @title Hunger
#' @description Calculates the share of people living in hunger.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level spatial aggregation. can be "iso","reg","regglo","glo"
#' @param after_shock FALSE is using the exogenous real income and the prices before a shock, TRUE is using the endogeenous real income that takes into account food price change on real income 
#' @param calibrated if calibrated is TRUE, kcal values are calibrated to better match historical years
#' @param share share of population that is undernourished
#' @return magpie object with hunger (mio people) or hunger share 
#' @author Benjamin Leon Bodirsky
#' @importFrom gdx readGDX
#' @importFrom magclass mbind
#' @examples
#' 
#'   \dontrun{
#'     x <- Hunger(gdx)
#'   }
#' 



Hunger<-function(gdx,level="reg",after_shock=TRUE,calibrated=FALSE,share=TRUE){
  warning("better use people underweight")
  kcal_pc<-Kcal(gdx = gdx,level="iso",products = "kfo",product_aggr = TRUE,after_shock = after_shock,calibrated=calibrated)
  hunger_shr <- 2674.855 * 0.997916997^kcal_pc / 100 
  hunger_shr[hunger_shr>1]<-1
  out=hunger_shr*population(gdx,level="iso")
  if(level=="reg"){
    mapping<-readGDX(gdx,"i_to_iso")
    out<-toolAggregate(out,rel=mapping,from="iso",to = "i")
  } else if(level=="glo"){
    out<-colSums(out)
  } else if (level=="regglo"){
    mapping<-readGDX(gdx,"i_to_iso")
    out<-toolAggregate(out,rel=mapping,from="iso",to = "i")
    out<-mbind(out,colSums(out))
  } else if (level!="iso"){stop("unknown level")}
  if(share){
    out<-out/population(gdx,level=level)
    out[is.na(out)]=0
    out[out==Inf]=0
  }
  return(out)
}