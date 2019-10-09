#' @title Seed
#' @description Calculates MAgPIE demand for Seed out of a gdx file 
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @details Demand definitions are equivalent to FAO CBS categories
#' @return demand as MAgPIE object (Unit depends on attributes)
#' @author Benjamin Leon Bodirsky
#' @importFrom magclass getRegions
#' @importFrom magclass add_dimension
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- demand(level="regglo", products="kcr")
#'   }
#' 

Seed<-function(gdx,level="reg",attributes="dm",spamfiledirectory=""){

  products=findset("kcr")
  seed<-readGDX(gdx = gdx, "ov_dem_seed", select = list(type="level"))[,,products]
  products <- getNames(seed)
  
  if(any(attributes!="dm")){
    att=readGDX(gdx,"fm_attributes")[,,attributes]
    seed<-seed*att[,,products]
  }
  out <- gdxAggregate(gdx = gdx,weight = 'production',x = seed,to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, products=products, product_aggr=FALSE)

}