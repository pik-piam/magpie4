#' @title reportManure
#' @description Reports the Nitrogen in Manure of all animals for future MAgPIE projections
#' 
#' @importFrom magpiesets reportingnames
#' @param nutrient nr, p, c...
#' @export
#' 
#' @param gdx GDX file
#' @author Benjamin Leon Bodirsky
#' @examples
#'   \dontrun{
#'     x <- reportManure(gdx)
#'   }
#' 

reportManure<-function(gdx,nutrient="nr"){
  manure<-collapseNames(readGDX(gdx,"ov_manure",select=list(type="level"))[,,nutrient])
  
  tmp0 = dimSums(manure,dim=3)
  tmp1 = dimSums(manure,dim=3.1)
  tmp2 = dimSums(manure,dim=3.2)
  
  
  names_tmp1<-reportingnames(getNames(tmp1))
  names_tmp2<-reportingnames(getNames(tmp2))
  names(names_tmp1)<-NULL
  names(names_tmp2)<-NULL
  unit=paste(toupper(substr(nutrient,1,1)),substring(nutrient,first = 2),sep = "")
  getNames(tmp0) <- paste0("Resources|",reportingnames(nutrient),"|Manure (Mt ",unit,"/yr)")
  getNames(tmp1) <- paste0("Resources|",reportingnames(nutrient),"|Manure|++|",names_tmp1," (Mt ",unit,"/yr)")
  getNames(tmp2) <- paste0("Resources|",reportingnames(nutrient),"|Manure|+|",names_tmp2," (Mt ",unit,"/yr)")
  tmp3<-dimSums(collapseNames(readGDX(gdx,"ov_manure_confinement")[,,nutrient][,,"level"]),dim=3.1)
  getNames(tmp3)<-paste0("Resources|",reportingnames(nutrient),"|Manure|",reportingnames("confinement"),"|+|",reportingnames(getNames(tmp3))," (Mt ",unit,"/yr)")
  
  out<-mbind(tmp0,tmp1,tmp2,tmp3)
  out <- gdxAggregate(gdx,out,from="reg",to="regglo")
  return(out)
}