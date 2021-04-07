#' @title AgGDP
#' @description Reads data to calculate the agricultural GDP
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing values related with overall value of production [million US$05]
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- AgGDP(gdx)
#'   }
#'

AgGDP <- function(gdx,file=NULL,level="reg"){
  
  if (level != "reg " | level != "regglo") stop("Only reg and regglo levels supported at the moment")
  
  
  prod_kcr<-production(gdx,level="reg",product_aggr = FALSE,products = "kall",attributes = "dm")
  prod_kli<-production(gdx,level="reg",product_aggr = FALSE,products = "kli",attributes = "dm")
  
  price_kcr<-prices(gdx, level="reg", products="kcr", product_aggr=FALSE, attributes="dm", type="producer")
  price_kli<-prices(gdx, level="reg", products="kli", product_aggr=FALSE, attributes="dm", type="producer")
  
  ValProd<-dimSums(prod_kcr*price_kcr+prod_kli*price_kli,dim=3)
  
  names_fas<-c("Demand|+|Seed (Mt DM/yr)",
               "Demand|Feed|+|Crops (Mt DM/yr)",
               "Demand|Feed|+|Secondary products (Mt DM/yr)",
               "Demand|Feed|+|Livestock products (Mt DM/yr)")
  
  demand_feed<-reportDemand(gdx)[,,names_fas]["GLO",,,invert=TRUE]
  price_seed<-prices(gdx, level="reg", products="kcr", product_aggr=TRUE, attributes="dm", type="consumer")
  price_kcr<-prices(gdx, level="reg", products="kcr", product_aggr=TRUE, attributes="dm", type="consumer")
  price_secondary<-prices(gdx, level="reg", products="kli", product_aggr=FALSE, attributes="dm", type="consumer")[,,"livst_milk"]
  price_kli<-prices(gdx, level="reg", products="kli", product_aggr=TRUE, attributes="dm", type="consumer")
  
  ValDemand<-demand_feed[,,1]*price_seed+
             demand_feed[,,2]*price_kcr+
             demand_feed[,,3]*price_secondary+
             demand_feed[,,4]*price_kli
  
  getNames(ValDemand)<-NULL
  
  out<-ValProd-ValDemand
  
  if (level == "regglo") out <- superAggregate(out, aggr_type = "sum", level = "regglo")
  
  getNames(out)<-"Agricultural GDP"
  
  out(out,file)
}