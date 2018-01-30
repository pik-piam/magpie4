#' @title demand
#' @description Calculates MAgPIE demand out of a gdx file 
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
#' @details Demand definitions are equivalent to FAO CBS categories
#' @return demand as MAgPIE object (Unit depends on attributes)
#' @author Benjamin Leon Bodirsky
#' @importFrom magclass add_dimension
#' @examples
#' 
#'   \dontrun{
#'     x <- demand(level="regglo", products="kcr")
#'   }
#' 

demand<-function(gdx,file=NULL,level="reg",products=readGDX(gdx,"kall"),product_aggr=FALSE,attributes="dm",type=NULL){
  if (!all(products%in%findset("kall"))){
    products<-findset("kall")
  }
  
  food<-readGDX(gdx = gdx, "ov_dem_food", select = list(type="level"))
  feed<-dimSums(readGDX(gdx = gdx, "ov_dem_feed", select = list(type="level")),dim="kap")
  processing<-readGDX(gdx = gdx, "ov_dem_processing", select = list(type="level"))
  material<-readGDX(gdx = gdx, "ov_dem_material", select = list(type="level"))
  if(is.null(material)){ ### can be removed at later stage (included for downwards comtability in jan18)
    material<-readGDX(gdx = gdx, "pm_dem_material")
  }
  bioenergy<-readGDX(gdx = gdx, "ov_dem_bioen", select = list(type="level"))
  seed<-readGDX(gdx = gdx, "ov_dem_seed", select = list(type="level"))
  waste<-readGDX(gdx = gdx, "ov16_dem_waste", select = list(type="level"))
  balanceflow<-readGDX(gdx = gdx, "f16_domestic_balanceflow")[,getYears(food),]
  
  out<-mbind(
    add_dimension(x = food,dim=3.1,add="demand",nm="food"),
    add_dimension(x = feed,dim=3.1,add="demand",nm="feed"),
    add_dimension(x = processing,dim=3.1,add="demand",nm="processed"),
    add_dimension(x = material,dim=3.1,add="demand",nm="other_util"),
    add_dimension(x = bioenergy,dim=3.1,add="demand",nm="bioenergy"),
    add_dimension(x = seed,dim=3.1,add="demand",nm="seed"),
    add_dimension(x = waste,dim=3.1,add="demand",nm="waste"),
    add_dimension(x = balanceflow,dim=3.1,add="demand",nm="dom_balanceflow")
  )
  #test
  supply<-readGDX(gdx = gdx, "ov_supply", select = list(type="level"))
  if(any(round(dimSums(out,dim="demand")-supply,4)!=0)) warning("Mismatch of ov_supply and sum of demand types.")
  
  out<-out[,,products]
  if(any(attributes!="dm")){
    att=readGDX(gdx,"fm_attributes")[,,products][,,attributes]
    out<-out*att
  }
  if(product_aggr){out<-dimSums(out,dim=3.2)}
  if(!is.null(type)) out <- out[,,type]
  if(level!="reg") out <- superAggregate(out,aggr_type="sum",level=level)
  out(out,file)
}