#' @title production
#' @description reads production out of a MAgPIE gdx file
#' 
#' @export
#' @importFrom magclass read.magpie
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @param water_aggr aggregate irrigated and non-irriagted production or not (boolean).
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @return production as MAgPIE object (unit depends on attributes)
#' @author Benjamin Leon Bodirsky
#' @seealso \code{\link{reportProduction}}, \code{\link{demand}}
#' @examples
#' 
#'   \dontrun{
#'     x <- production(gdx)
#'   }
#' 

production<-function(gdx,file=NULL,level="reg",products="kall",product_aggr=FALSE,attributes="dm",water_aggr=TRUE,spamfiledirectory=""){
  
  if (!all(products%in%findset("kall"))){
    products<-readGDX(gdx,products)
  }
  
  if(level%in% c("glo","reg","regglo")){
    if (water_aggr) {
      production <- readGDX(gdx,"ov_prod_reg",select=list(type="level"))
    } else {
      if(!all(products%in%findset("kcr"))){stop("Irrigation only exists for production of kcr products")}
      area <- readGDX(gdx,"ov_area",select=list(type="level"))[,,products]
      yield <- readGDX(gdx,"ov_yld",select=list(type="level"))[,,products]
      production <- area*yield
      production <- superAggregate(production,aggr_type = "sum",level = "reg")
    }
  } else if (level%in%c("cell")) {
    if(all(products%in%findset("kcr"))){
      if (water_aggr) {
        if(!all(products%in%findset("kcr"))){stop("Irrigation only exists for production of kcr products")}
        production <- readGDX(gdx,"ov_prod",select=list(type="level"))[,,products]
      } else {
        area <- readGDX(gdx,"ov_area",select=list(type="level"))[,,products]
        yield <- readGDX(gdx,"ov_yld",select=list(type="level"))[,,products]
        production <- area*yield
      }
    } else if (all(products%in%findset("kres"))&all(findset("kres")%in%products)){
      production <- ResidueBiomass(gdx = gdx,level = level,products = "kcr",product_aggr = "kres",attributes = attributes,water_aggr = water_aggr)
    } else {
      stop("Cellular production only exists for production of kcr and kres products")
    }  
    
  } else if (level=="grid") {
    ## memory size problems. disaggregate product by product
    combined<-list()
    
    if(all(products%in%findset("kcr"))){
      #disaggregation for crops
      yields=read.magpie(path(spamfiledirectory,"lpj_yields_0.5.mz"))
      for(product_x in products){
        area<-croparea(gdx=gdx,level = "grid",products=product_x,water_aggr = FALSE,product_aggr=FALSE)
        yield<-yields[,getYears(area),product_x]
        
        warning("quickfix because of different cellnames! Has to be removed")
        dimnames(yield)[[1]]<-dimnames(area)[[1]]
        
        production <- area*yield
        if(product_aggr){production<-dimSums(production,dim=3.1)}
        if(water_aggr){production<-dimSums(production,dim=3.2)}  
        if(level=="grid"){
          weight=production
        }else {weight=NULL}
        combined[[product_x]]<-gdxAggregate(gdx=gdx,
              x = production(gdx=gdx,level="cell",products=product_x,product_aggr=product_aggr,attributes=attributes,water_aggr=water_aggr),
              weight = weight,absolute = TRUE,to = "grid")
      }
    } else if (all(products%in%findset("kres"))&all(findset("kres")%in%products)){
      #disaggregation for crop residues
      production<-gdxAggregate(
        gdx=gdx,
        x = production(gdx=gdx,level="cell",products=products,product_aggr=FALSE,attributes=attributes,water_aggr=water_aggr),
        weight = "ResidueBiomass", product_aggr="kres",attributes="dm",
        absolute = TRUE,to = "grid")
    } else {stop("Gridded production so far only exists for production of kcr and kres products")}
    production<-mbind(combined)
  } else {
    stop(paste0("Level ",level," does not exist yet."))
  }

  out<-production[,,products]
  if(any(attributes!="dm")){
    att=readGDX(gdx,"fm_attributes")[,,products][,,attributes]
    out<-out*att
  }
  if(product_aggr){out<-dimSums(out,dim=3.1)}
  
  
  out<-gdxAggregate(gdx=gdx,x=out,weight = NULL, to = level,absolute = T,
                    spamfiledirectory = spamfiledirectory,products=products,product_aggr=product_aggr,water_aggr=water_aggr)
  out(out,file)
}






  

  

    

  