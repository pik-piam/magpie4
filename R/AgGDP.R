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
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- AgGDP(gdx)
#'   }
#'

AgGDP <- function(gdx,file=NULL,level="reg"){

  if (!(level %in% c("reg","regglo"))) stop("Only reg and regglo levels supported at the moment")


  prod_kcr<-production(gdx,level="reg",product_aggr = FALSE,products = "kcr",attributes = "dm")
  prod_kli<-production(gdx,level="reg",product_aggr = FALSE,products = "kli",attributes = "dm")

  price_kcr<-prices(gdx, level="reg", products="kcr", product_aggr=FALSE, attributes="dm", type="producer")
  price_kli<-prices(gdx, level="reg", products="kli", product_aggr=FALSE, attributes="dm", type="producer")

  ValProd<-dimSums(prod_kcr*price_kcr,dim=3)+dimSums(prod_kli*price_kli,dim=3)

  names_fas<-c("seed",
               "feed")

  demand_kcr<-dimSums(demand(gdx,products = c("kcr"),level="reg")[,,c("feed","seed")],dim=3.1)
  demand_kli<-dimSums(demand(gdx,products = c("kli"),level="reg")[,,c("feed","seed")],dim=3.1)

  price_kcr_con<-prices(gdx, level="reg", products="kcr", product_aggr=FALSE, attributes="dm", type="consumer")
  price_kli_con<-prices(gdx, level="reg", products="kli", product_aggr=FALSE, attributes="dm", type="consumer")

  ValDemand<-dimSums(demand_kcr*price_kcr_con,dim=3)+dimSums(demand_kli*price_kli_con,dim=3)

  out<-ValProd-ValDemand

  if (level == "regglo") out <- superAggregate(out, aggr_type = "sum", level = "regglo")

  getNames(out)<-"Agriculture GDP"

  out(out,file)
}
