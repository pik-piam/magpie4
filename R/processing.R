#' @title processing
#' @description Calculates MAgPIE disaggregated processing out of a gdx file 
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @param type Demand type(s): "Food", "Feed", "Processing", "Material", "Bioenergy", "Seed", "Supply chain loss", "Domestic Balanceflow"; NULL returns all types 
#' @param indicator process or secondary product output
#' @details Demand definitions are equivalent to FAO CBS categories
#' @return processing as MAgPIE object (Unit depends on attributes)
#' @author David Chen, Benjamin Leon Bodirsky
#' @importFrom magclass add_dimension dimOrder
#' @examples
#' 
#'   \dontrun{
#'     x <- processing(gdx = gdx, level="regglo", products="kcr", indicator="primary_to_process")
#'   }
#'
processing<-function(gdx,file=NULL,level="reg",products=readGDX(gdx,"kall"),product_aggr=FALSE,attributes="dm",type=NULL, indicator = "secondary_from_primary"){
  
  
  if (!all(products%in%findset("kall"))){
  products<-findset("kall")
}

if(indicator=="primary_to_process"){   
    
#primary into process
processdemand<-readGDX(gdx= gdx, "ov20_dem_processing", select = list(type="level"))

out<- dimOrder(processdemand, c(1,2))

}
#primary to secondary

else if(indicator == "secondary_from_primary"){
processdemand<-readGDX(gdx= gdx, "ov20_dem_processing", select = list(type="level"))
  
conv_factors <- readGDX(gdx = gdx, "f20_processing_conversion_factors")

years = getYears(processdemand) 
conv_factors1<- conv_factors[,years,]

out<- processdemand * dimSums(conv_factors1, dim=3.1)
out<- dimSums(out, dim = 3.1)

}
else (stop ("unknown indicator"))
  return(out)
}



