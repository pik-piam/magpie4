#' @title reportSDG15
#' @description reports all SDG indicators relevant for SD15 - Life on Land
#' @import magpiesets
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportSDG15(gdx)
#'   }
#' 

reportSDG15 <- function(gdx) {
  x <- NULL
  
  indicatorname="SDG|SDG15|Forest share"	
  unit="share of total land"
  out <- land(gdx,level="regglo")
  out<- dimSums(out[,,c("forestry","primforest","secdforest")])/dimSums(out)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG15|Primary forest share"	
  unit="share of total land"
  out <- land(gdx,level="regglo")
  out<- dimSums(out[,,c("primforest")])/dimSums(out)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG15|Biodiversity protection proportion"	
  unit="share of total land"
  #p35_save_primforest / vm_land.l(j,"primforest") 
  out <- land(gdx,level="regglo",types = NULL,subcategories = c("primforest","forestry","secdforest","other"),sum = FALSE)
  out<- dimSums(out[,,c("prot")])/dimSums(out)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG15|Afforestation" 
  unit="million ha"
  out <- land(gdx,level="regglo",types = NULL,subcategories = c("forestry"),sum = FALSE)
  out <- dimSums(out[,,c("new","prot","grow")])
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG15|Other natural land"	
  unit="share of total land"
  out <- land(gdx,level="regglo")
  out<- dimSums(out[,,c("other")])/dimSums(out)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG15|Terrestrial biodiversity"	
  unit="index"
  #out <- land(gdx,level="regglo")
  #getNames(out) <- paste0(indicatorname, " (",unit,")")
  #x <- mbind(x,out)
  
  indicatorname="SDG|SDG15|Freshwater biodiversity"	
  unit="index"
  #out <- land(gdx,level="regglo")
  #getNames(out) <- paste0(indicatorname, " (",unit,")")
  #x <- mbind(x,out)
  
  indicatorname="SDG|SDG15|Non-agricultural land"	
  unit="share of total land"
  out <- land(gdx,level="regglo")
  out<- dimSums(out[,,c("forestry","primforest","secdforest","urban","other")])/dimSums(out)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  #x <- x[,,sort(getNames(x))]  
  return(x)
}

