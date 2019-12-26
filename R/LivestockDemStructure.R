#' @title LivestockDemStructure
#' @description Calculates the share of different livestock commodities in total livestock product consumption on the basis of chosen attribute
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)
#' @param after_shock FALSE is using the exogenous real income and the prices before a shock, TRUE is using the endogeenous real income that takes into account food price change on real income
#' @param calibrated if FALSE, the true regression outputs are used, if TRUE the values calibrated to the start years are used
#' @param attributes unit: kilocalories per day ("kcal"), g protein per day ("protein"). Mt reactive nitrogen ("nr").
#' @param fish if true, livestock share includes fish, otherwhise not
#' 
#' @return magpie object with the livestock demand structure in a region or country. Unit is dimensionsless, but value depends on chosen attribute
#' @author Isabelle Weindl
#' @importFrom magpiesets findset
#' @importFrom magclass dimSums
#' @examples
#' 
#'   \dontrun{
#'     x <- LivestockDemStructure(gdx)
#'   }
#' 

LivestockDemStructure<- function(gdx, 
                          file=NULL, 
                          level="reg", 
                          after_shock=TRUE, 
                          calibrated=TRUE,
                          attributes="kcal",
                          fish=FALSE){
  
  if (fish==TRUE){
    products=findset("kap")
  } else if (fish==FALSE){
    products=findset("kli")
  }else{stop("fish has to be binary")}
  
  kcal<-Kcal(gdx, level=level, products="kfo", 
             product_aggr=FALSE, 
             after_shock=after_shock, 
             calibrated=calibrated,
             attributes=attributes,
             per_capita=TRUE)
  out<-kcal[,,products]/dimSums(kcal[,,products],dim=3.1)
  
  out(out,file)
}
