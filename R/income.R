#' @title income
#' @description Calculates income based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param per_capita income per capita or aggregated for the total population 
#' @param type ppp for purchase power parity, mer for market exchange rate
#' @param after_shock FALSE is using the exogenous real income, TRUE is using the endogeenous real income that takes into account food price change on real income
#' @return annual income as MAgPIE object (unit depends on per_capita: US$2005 MER/cap/yr (TRUE), US$2005 MER/yr (FALSE))
#' @author Florian Humpenoeder, Benjamin Bodirsky
#' @importFrom magclass colSums
#' @examples
#' 
#'   \dontrun{
#'     x <- income(gdx)
#'   }
#' 

income <- function(gdx, file=NULL, level="reg", per_capita=TRUE, type="ppp", after_shock=FALSE) {
  if (after_shock==TRUE){
    if(type=="ppp"){
      gdp_pc<-readGDX(gdx=gdx,"ov15_income_pc_real_ppp_iso",select=list(type="level"))  
    } else {
      stop("after shock only available for ppp so far.")
    }
    pop<-population(gdx,level="iso")
    gdp=gdp_pc*pop
  } else if (after_shock==FALSE){
    if(type=="ppp"){
      gdp<-readGDX(gdx=gdx,"i09_gdp_ppp_iso")[,readGDX(gdx,"t"),]
    } else if (type=="mer"){
      gdp<-readGDX(gdx=gdx,"i09_gdp_mer_iso")[,readGDX(gdx,"t"),]
    }else {
      stop("type has to be mer or ppp")
    }
  } else (stop("after_shock has to be binary"))
  
  if (level=="reg"){
    mapping<-readGDX(gdx,"i_to_iso")
    gdp<-speed_aggregate(x=gdp,rel = mapping,from = "iso",to="i",dim = 1)
  } else if(level=="glo"){
    gdp<-colSums(gdp)
  } else if (level=="regglo") {
    mapping<-readGDX(gdx,"i_to_iso")
    gdp<-speed_aggregate(x=gdp,rel = mapping,from = "iso",to="i",dim = 1)
    gdp<-mbind(gdp,colSums(gdp))
  } else if (level!="iso"){stop("unkown level")}
  
  if(per_capita==TRUE){
    out=gdp/population(gdx,level=level)
  } else {
    out=gdp
  }
  
  out(out,file)
}
