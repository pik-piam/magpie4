#' @title trade
#' @description Calculates MAgPIE trade or self-sufficiencies out of a gdx file 
#' 
#' @importFrom magclass where
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @param weight in case relative=T also the weighting for the self sufficiencies is provided as it is an intensive parameter
#' @param relative if relative=TRUE, self sufficiencies are reported, so the amount of production divided by domestic demand
#' @param type exports-imports ("net-exports"), gross imports ("imports") or gross exports ("exports"); only valid if relative=FALSE
#' @details Trade definitions are equivalent to FAO CBS categories
#' @return trade (production-demand) as MAgPIE object; unit depends on attributes
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder, Mishko Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- trade(gdx="fulldata.gdx", level="regglo", products="kcr")
#'   }
#' 
#' @importFrom gdx expand.set

trade<-function(gdx,file=NULL,level="reg",products = "k_trade",product_aggr=FALSE,attributes="dm",weight=FALSE,relative=FALSE,type="net-exports") {

  if (!all(products%in%readGDX(gdx,"kall"))){
    products <- try(readGDX(gdx,products))
    if(is.null(products)){
      products <- expand.set(gdx, "kall")
      warning("The specified commodity set in products argument does not exit. 
              Instead the full kall set is given to products argument.")
    }
  }
  
  production<-production(gdx,level=level,products=products,product_aggr=FALSE,attributes=attributes)

  demand <- dimSums(demand(gdx,level=level,products=products,product_aggr=FALSE,attributes=attributes),dim=3.1)

  
  ## The messages below seem to get triggered by extremely low values in diff. 
  ## Could be a rounding issue. Rounding to 7 digits should be safe because we deal in 10e6 values mostly.
  diff <- round(production(gdx,level="glo")-dimSums(demand(gdx,level="glo"),dim=3.1),7)
  balanceflow <- readGDX(gdx,"f21_trade_balanceflow",react = "silent")
  if(is.null(balanceflow)) {
    balanceflow <- readGDX(gdx,"fm_trade_balanceflow",react = "silent") ## Needs to be converted to interface for timber module WIP
  }
 
  balanceflow <- balanceflow[,getYears(diff),]
  diff <- diff[,,getNames(balanceflow)] - balanceflow
 
  if(any(round(diff,2)>0)) {
    message("\nFor the following categories, overproduction is noticed (on top of balanceflow): \n",paste(unique(as.vector(where(round(diff,2)>0)$true$individual[,3])),collapse=", "),"\n")
  }
  if(any(round(diff,2)<0)) {
    warning("For the following categories, underproduction (on top of balanceflow): \n",paste(unique(as.vector(where(round(diff,2)<0)$true$individual[,3])),collapse=", "),"\n")
  }
  proddem<-mbind(
    add_dimension(production,dim=3.1,add="type",nm="production"),
    add_dimension(demand,dim=3.1,add="type",nm="demand")
    )
  if (relative) {
    if(product_aggr){
      proddem<-dimSums(proddem,dim="kall")
    }
    if (weight) {
      out<-list(x=dimSums(proddem[,,"production"],dim=3.1)/dimSums(proddem[,,"demand"],dim=3.1),weight=dimSums(proddem[,,"demand"],dim=3.1))
    } else {
      out<-dimSums(proddem[,,"production"],dim=3.1)/dimSums(proddem[,,"demand"],dim=3.1)
    }
  } else {
    out <- dimSums(proddem[,,"production"],dim=3.1)-dimSums(proddem[,,"demand"],dim=3.1)
    if(type == "net-exports"){
      if(product_aggr){
        out<-dimSums(out,dim="kall")
      }
    } else if (type=="exports") {
      out[out<0] <- 0
      if(product_aggr){
        out<-dimSums(out,dim="kall")
      }
    } else if(type=="imports") {
      out[out>0] <- 0
      out <- -1*out
      if(product_aggr){
        out<-dimSums(out,dim="kall")
      }
    } else {stop("unknown type")}
    if (weight) {
      out<-list(x=out,weight=NULL)
    } else {
      out<-out
    }
  }

  if (is.list(out)) {return(out)} else out(out,file)
}