#' @title consumValue
#' @description calculates consumption value based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param type Demand type(s): "Food", "Feed", "Processing", "Material", "Bioenergy", "Seed", "Supply chain loss", "Domestic Balanceflow"; NULL returns all types 
#' @param expenditure_shr logical indicator if the output of the function is represented as a expenditure (consumption) share of GDP for the indicated type of demand. Default is \code{FALSE}.
#' @details This function just multiplies the demand with the consumers' prices. For some commodities regional prices could be NA or NAN. These are set to zero in the function. The function can also return expenditure shares in GDP for given demand types.
#' @return A MAgPIE object containing the consumption value in million US$05, or a percentage of GDP if expenditure share is flagged as \code{TRUE}.
#' @author Mishko Stevanovic, Florian Humpenoeder
#' @importFrom magclass as.magpie dimSums
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- consumValue(gdx)
#'   }
#' 

consumValue <- function(gdx, file=NULL, level="reg", products="kall", product_aggr=FALSE, type="food", expenditure_shr=FALSE){
  
  if (!all(products%in%findset("kall"))) products<-readGDX(gdx,"kall")
  
  dem   <- demand(gdx, level="reg", products=products,type=type)
  p_dem <- prices(gdx, level="reg", products=products)
  
  # checks if some prices are equal to NaN or NA 
  nas <- which(is.na(p_dem), arr.ind=TRUE)
  if(dim(nas)[1]!=0) p_dem[nas] <- 0
  
  out <- as.magpie(dem*p_dem)
  # aggregate over regions
  if(level!="reg") out <- superAggregate(out,aggr_type="sum",level=level, na.rm=TRUE)
  # aggregate over products
  if(product_aggr) out <- dimSums(out,na.rm=TRUE,dim=3.2)
  out <- out
  
  # calculate expenditure share (total expenditures over total income)
  if(expenditure_shr) out <- out/income(gdx,level=level,per_capita=FALSE)

  out(out, file)
}