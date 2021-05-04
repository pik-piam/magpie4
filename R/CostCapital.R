#' @title CostCapital
#' @description Reads data to calculate capital stocks
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
#'     x <- CostCapital(gdx)
#'   }
#'

CostCapital <- function(gdx,file=NULL,level="cell"){
  
  #Reads existing capital in each time step
  
  pre_capital_im <- if(!is.null(suppressWarnings(readGDX(gdx,"p38_capital_immobile")))) dimSums(readGDX(gdx,"p38_capital_immobile"),dim=3) else NULL
  pre_capital_mo <- if(!is.null(suppressWarnings(readGDX(gdx,"p38_capital_mobile"))))  readGDX(gdx,"p38_capital_mobile") else NULL
  
  # Mixed and PerTon factor costs realizations don't contain capital info. 
  # Check that stops the function in case capital is not accounted for
  if (any(is.null(pre_capital_im), is.null(pre_capital_mo))) stop("Capital stocks only available for the sticky factor costs realization")
  
  Sum_stocks <- pre_capital_im + pre_capital_mo
  
  getNames(Sum_stocks) <- "Capital Stocks"
  

  weight<- NULL  
  
  if (level != "cell") Sum_stocks <- superAggregate(Sum_stocks, aggr_type = "sum", level = level)
  out <- Sum_stocks
  
  out(out,file)
}
