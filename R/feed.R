#' @title feed
#' @description calculates feed demand by animal type out of a gdx file
#
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param detail if FALSE, only total feed demand per animal type is calculated without details on the type of feed
#' @param nutrient The nutrient in which the results shall be calculated
#' @return feed demand by animal type as MAgPIE object (unit depends on selected nutrient attributes)
#' @author Isabelle Weindl
#' @importFrom gdx readGDX out
#' @importFrom magclass dimSums
#' @importFrom luscale superAggregate
#' @export
#' @examples
#'   \dontrun{
#'     x <- feed(gdx)
#'   }
#' 
feed<-function(gdx,file=NULL,level="reg", detail=T, nutrient="dm"){
  
    feed<-readGDX(gdx = gdx, "ov_dem_feed", select = list(type="level"))
    feed_types<-getNames(feed,dim=2)
#    animal_types<-getNames(feed,dim=1)
    
#    kap_production <- readGDX(gdx,"ov_prod_reg",select=list(type="level"))[,,animal_types]
#    weight <- NULL
#    for(i in feed_types){
#      weight <- mbind(weight,setNames(kap_production,paste(getNames(kap_production),i,sep=".")))
#    }
  
    att=readGDX(gdx,"fm_attributes")[,,feed_types][,,nutrient]
    feed<-collapseNames(feed*att)
    
    if(!detail){feed<-dimSums(feed,dim=3.2)}
    if(level!="reg"){feed <- superAggregate(feed,aggr_type="sum", level=level)}
    out(feed,file)
}